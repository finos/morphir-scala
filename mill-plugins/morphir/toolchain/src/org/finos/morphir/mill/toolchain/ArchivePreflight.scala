package org.finos.morphir.mill.toolchain

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import java.util.zip.GZIPInputStream
import scala.util.Using
import scala.util.control.NonFatal

private[toolchain] object ArchivePreflight {
  private val ZipEndSignature    = 0x06054b50
  private val Zip64EndSignature  = 0x06064b50
  private val Zip64Locator       = 0x07064b50
  private val ZipEndMinimumBytes = 22
  private val ZipMaximumComment  = 65535
  private val TarBlockBytes      = 512
  private val TarSizeOffset      = 124
  private val TarSizeLength      = 12
  private val TarChecksumOffset  = 148
  private val TarChecksumLength  = 8
  private val TarTypeOffset      = 156

  def tarGz(archive: os.Path, limits: ArchiveLimits): Unit =
    try
      Using.resource(Files.newInputStream(archive.toNIO)) { raw =>
        Using.resource(new GZIPInputStream(raw)) { gzip =>
          scanTar(gzip, archive, limits)
        }
      }
    catch {
      case error: IllegalArgumentException => throw error
      case NonFatal(error)                 =>
        throw new IllegalArgumentException(s"Malformed or truncated TAR.GZ archive: $archive", error)
    }

  def zip(archive: os.Path, limits: ArchiveLimits): Unit =
    Using.resource(FileChannel.open(archive.toNIO, StandardOpenOption.READ)) { channel =>
      val fileSize = channel.size()
      if (fileSize < ZipEndMinimumBytes)
        fail(s"ZIP is too short to contain an end-of-central-directory record: $archive")
      val tailSize  = math.min(fileSize, ZipEndMinimumBytes.toLong + ZipMaximumComment).toInt
      val tailStart = fileSize - tailSize
      val tail      = ByteBuffer.allocate(tailSize).order(ByteOrder.LITTLE_ENDIAN)
      readFully(channel, tail, tailStart, "ZIP end-of-central-directory")
      val endOffset = locateZipEnd(tail, tailStart, fileSize, archive)
      val relative  = (endOffset - tailStart).toInt
      val disk      = unsignedShort(tail, relative + 4)
      val startDisk = unsignedShort(tail, relative + 6)
      val diskCount = unsignedShort(tail, relative + 8)
      val count     = unsignedShort(tail, relative + 10)
      if (disk != 0 || startDisk != 0)
        fail(s"Multi-disk ZIP archives are not supported: $archive")
      val centralBytes  = unsignedInt(tail, relative + 12)
      val centralOffset = unsignedInt(tail, relative + 16)
      val zip64Required =
        diskCount == 0xffff || count == 0xffff || centralBytes == 0xffffffffL || centralOffset == 0xffffffffL
      val directory =
        if (zip64Required) readZip64(channel, endOffset, fileSize, archive)
        else {
          if (diskCount != count) fail(s"Multi-disk ZIP archives are not supported: $archive")
          ZipDirectory(count.toLong, centralBytes, centralOffset, endOffset)
        }
      validateZipDirectory(directory, fileSize, archive, limits)
    }

  private def scanTar(input: InputStream, archive: os.Path, limits: ArchiveLimits): Unit = {
    val header            = new Array[Byte](TarBlockBytes)
    val skipBuffer        = new Array[Byte](8192)
    val maxStreamBytes    = derivedTarStreamLimit(limits)
    var streamBytes       = 0L
    var entryCount        = 0L
    var contentBytes      = 0L
    var metadataBytes     = 0L
    var previousZeroBlock = false
    var finished          = false

    while (!finished) {
      readExactly(input, header, 0, header.length, "TAR header", archive)
      streamBytes = addWithin(streamBytes, header.length, maxStreamBytes, "TAR decompressed stream", archive)
      val zero = header.forall(_ == 0)
      if (zero && previousZeroBlock) finished = true
      else if (zero) previousZeroBlock = true
      else {
        if (previousZeroBlock)
          fail(s"Malformed TAR archive has an entry after an end marker: $archive")
        validateTarChecksum(header, archive)
        entryCount = addWithin(entryCount, 1L, limits.maxEntries, "TAR header and pseudo-entry count", archive)
        val size      = parseTarNumber(header, TarSizeOffset, TarSizeLength, "TAR entry size", archive)
        val entryType = header(TarTypeOffset)
        if (isTarMetadata(entryType)) {
          if (size > limits.maxMetadataEntryBytes)
            fail(s"TAR metadata entry byte limit ${limits.maxMetadataEntryBytes} exceeded by $size bytes: $archive")
          metadataBytes = addWithin(
            metadataBytes,
            size,
            limits.maxArchiveMetadataBytes,
            "TAR aggregate metadata bytes",
            archive
          )
        } else {
          if (size > limits.maxEntryUncompressedBytes)
            fail(s"TAR per-entry uncompressed byte limit ${limits.maxEntryUncompressedBytes} exceeded: $archive")
          contentBytes = addWithin(
            contentBytes,
            size,
            limits.maxTotalUncompressedBytes,
            "TAR total uncompressed bytes",
            archive
          )
        }
        val padding = (TarBlockBytes - size % TarBlockBytes) % TarBlockBytes
        val body    = checkedAdd(size, padding, "TAR entry body and padding", archive)
        skipExactly(input, body, skipBuffer, archive)
        streamBytes = addWithin(streamBytes, body, maxStreamBytes, "TAR decompressed stream", archive)
      }
    }

    var read = input.read(skipBuffer)
    while (read >= 0) {
      if (read > 0) {
        streamBytes = addWithin(streamBytes, read.toLong, maxStreamBytes, "TAR decompressed stream", archive)
        var index = 0
        while (index < read) {
          if (skipBuffer(index) != 0)
            fail(s"Malformed TAR archive has non-zero bytes after its end markers: $archive")
          index += 1
        }
      }
      read = input.read(skipBuffer)
    }
  }

  private def validateTarChecksum(header: Array[Byte], archive: os.Path): Unit = {
    val stored      = parseTarOctal(header, TarChecksumOffset, TarChecksumLength, "TAR checksum", archive)
    var unsignedSum = 0L
    var signedSum   = 0L
    var index       = 0
    while (index < header.length) {
      val unsigned = if (index >= TarChecksumOffset && index < TarChecksumOffset + TarChecksumLength) 32
      else header(index) & 0xff
      val signed = if (index >= TarChecksumOffset && index < TarChecksumOffset + TarChecksumLength) 32
      else header(index).toInt
      unsignedSum += unsigned
      signedSum += signed
      index += 1
    }
    if (stored != unsignedSum && stored != signedSum)
      fail(s"Malformed TAR header checksum: $archive")
  }

  private def parseTarNumber(
      bytes: Array[Byte],
      offset: Int,
      length: Int,
      label: String,
      archive: os.Path
  ): Long =
    if ((bytes(offset) & 0x80) != 0) parseTarBase256(bytes, offset, length, label, archive)
    else parseTarOctal(bytes, offset, length, label, archive)

  private def parseTarBase256(
      bytes: Array[Byte],
      offset: Int,
      length: Int,
      label: String,
      archive: os.Path
  ): Long = {
    if ((bytes(offset) & 0x40) != 0) fail(s"Negative $label is not supported: $archive")
    var value = (bytes(offset) & 0x3f).toLong
    var index = offset + 1
    while (index < offset + length) {
      if (value > (Long.MaxValue - (bytes(index) & 0xff)) / 256)
        fail(s"$label exceeds the supported signed 64-bit range: $archive")
      value = value * 256 + (bytes(index) & 0xff)
      index += 1
    }
    value
  }

  private def parseTarOctal(
      bytes: Array[Byte],
      offset: Int,
      length: Int,
      label: String,
      archive: os.Path
  ): Long = {
    val end   = offset + length
    var index = offset
    while (index < end && (bytes(index) == 0 || bytes(index) == ' '.toByte)) index += 1
    var value      = 0L
    var sawDigit   = false
    var terminated = false
    while (index < end) {
      val byte = bytes(index)
      if (byte >= '0'.toByte && byte <= '7'.toByte && !terminated) {
        val digit = byte - '0'.toByte
        if (value > (Long.MaxValue - digit) / 8)
          fail(s"$label exceeds the supported signed 64-bit range: $archive")
        value = value * 8 + digit
        sawDigit = true
      } else if (byte == 0 || byte == ' '.toByte) terminated = true
      else fail(s"Malformed octal $label: $archive")
      index += 1
    }
    if (!sawDigit) 0L else value
  }

  private def isTarMetadata(entryType: Byte): Boolean =
    entryType == 'L'.toByte || entryType == 'K'.toByte || entryType == 'x'.toByte || entryType == 'g'.toByte

  private def readExactly(
      input: InputStream,
      bytes: Array[Byte],
      offset: Int,
      length: Int,
      label: String,
      archive: os.Path
  ): Unit = {
    var position = offset
    val end      = offset + length
    while (position < end) {
      val read = input.read(bytes, position, end - position)
      if (read < 0) fail(s"Malformed or truncated $label: $archive")
      if (read == 0) fail(s"Unable to read $label: $archive")
      position += read
    }
  }

  private def skipExactly(input: InputStream, count: Long, buffer: Array[Byte], archive: os.Path): Unit = {
    var remaining = count
    while (remaining > 0) {
      val requested = math.min(remaining, buffer.length.toLong).toInt
      val read      = input.read(buffer, 0, requested)
      if (read < 0) fail(s"Malformed or truncated TAR entry body: $archive")
      if (read == 0) fail(s"Unable to read TAR entry body: $archive")
      remaining -= read
    }
  }

  private def derivedTarStreamLimit(limits: ArchiveLimits): Long = {
    val headerAndPadding = saturatingMultiply(limits.maxEntries, 1023L)
    saturatingAdd(
      saturatingAdd(limits.maxTotalUncompressedBytes, limits.maxArchiveMetadataBytes),
      saturatingAdd(headerAndPadding, 1024L)
    )
  }

  private def saturatingMultiply(left: Long, right: Long): Long =
    if (left > Long.MaxValue / right) Long.MaxValue else left * right

  private def saturatingAdd(left: Long, right: Long): Long =
    if (left > Long.MaxValue - right) Long.MaxValue else left + right

  private def checkedAdd(left: Long, right: Long, label: String, archive: os.Path): Long =
    if (left > Long.MaxValue - right) fail(s"$label exceeds the supported signed 64-bit range: $archive")
    else left + right

  private def addWithin(current: Long, increment: Long, limit: Long, label: String, archive: os.Path): Long =
    if (increment > limit - current) fail(s"$label exceeds limit $limit: $archive")
    else current + increment

  private final case class ZipDirectory(entries: Long, bytes: Long, offset: Long, structuralOffset: Long)

  private def readZip64(
      channel: FileChannel,
      endOffset: Long,
      fileSize: Long,
      archive: os.Path
  ): ZipDirectory = {
    val locatorOffset = endOffset - 20
    if (locatorOffset < 0) fail(s"ZIP64 locator is missing: $archive")
    val locator = readBuffer(channel, locatorOffset, 20, "ZIP64 locator")
    if (locator.getInt(0) != Zip64Locator) fail(s"ZIP64 locator is missing or malformed: $archive")
    val zip64Disk   = unsignedInt(locator, 4)
    val zip64Offset = unsignedLong(locator, 8, "ZIP64 end record offset")
    val totalDisks  = unsignedInt(locator, 16)
    if (zip64Disk != 0 || totalDisks != 1)
      fail(s"Multi-disk ZIP64 archives are not supported: $archive")
    if (zip64Offset > fileSize - 56)
      fail(s"ZIP64 end-of-central-directory offset is outside the archive: $archive")
    val record     = readBuffer(channel, zip64Offset, 56, "ZIP64 end-of-central-directory record")
    val signature  = record.getInt(0)
    val recordSize = unsignedLong(record, 4, "ZIP64 end record size")
    if (signature != Zip64EndSignature || recordSize < 44)
      fail(s"ZIP64 end-of-central-directory record is malformed: $archive")
    if (recordSize > locatorOffset - zip64Offset - 12 || zip64Offset + 12 + recordSize != locatorOffset)
      fail(s"ZIP64 end-of-central-directory record has an invalid size: $archive")
    val disk        = unsignedInt(record, 16)
    val startDisk   = unsignedInt(record, 20)
    val diskEntries = unsignedLong(record, 24, "ZIP64 disk entry count")
    val entries     = unsignedLong(record, 32, "ZIP64 total entry count")
    val bytes       = unsignedLong(record, 40, "ZIP64 central-directory size")
    val offset      = unsignedLong(record, 48, "ZIP64 central-directory offset")
    if (disk != 0 || startDisk != 0 || diskEntries != entries)
      fail(s"Multi-disk ZIP64 archives are not supported: $archive")
    ZipDirectory(entries, bytes, offset, zip64Offset)
  }

  private def locateZipEnd(tail: ByteBuffer, tailStart: Long, fileSize: Long, archive: os.Path): Long = {
    var index = tail.limit() - ZipEndMinimumBytes
    while (index >= 0) {
      if (tail.getInt(index) == ZipEndSignature) {
        val commentBytes = unsignedShort(tail, index + 20)
        val absolute     = tailStart + index
        if (absolute + ZipEndMinimumBytes + commentBytes == fileSize) return absolute
      }
      index -= 1
    }
    fail(s"ZIP end-of-central-directory record is missing or malformed: $archive")
  }

  private def validateZipDirectory(
      directory: ZipDirectory,
      fileSize: Long,
      archive: os.Path,
      limits: ArchiveLimits
  ): Unit = {
    if (directory.entries > limits.maxEntries)
      fail(s"ZIP entry count ${directory.entries} exceeds limit ${limits.maxEntries}: $archive")
    if (directory.bytes > limits.maxCentralDirectoryBytes)
      fail(
        s"ZIP central-directory byte size ${directory.bytes} exceeds limit ${limits.maxCentralDirectoryBytes}: $archive"
      )
    if (directory.offset > fileSize || directory.bytes > fileSize - directory.offset)
      fail(s"ZIP central-directory range is outside the archive: $archive")
    if (directory.offset + directory.bytes > directory.structuralOffset)
      fail(s"ZIP central-directory overlaps its end records: $archive")
  }

  private def readBuffer(channel: FileChannel, position: Long, size: Int, label: String): ByteBuffer = {
    val buffer = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN)
    readFully(channel, buffer, position, label)
    buffer
  }

  private def readFully(channel: FileChannel, buffer: ByteBuffer, position: Long, label: String): Unit = {
    var offset = position
    while (buffer.hasRemaining) {
      val read = channel.read(buffer, offset)
      if (read < 0) fail(s"Truncated $label")
      if (read == 0) fail(s"Unable to read $label")
      offset += read
    }
    buffer.flip()
  }

  private def unsignedShort(buffer: ByteBuffer, offset: Int): Int = buffer.getShort(offset) & 0xffff

  private def unsignedInt(buffer: ByteBuffer, offset: Int): Long =
    Integer.toUnsignedLong(buffer.getInt(offset))

  private def unsignedLong(buffer: ByteBuffer, offset: Int, label: String): Long = {
    val value = buffer.getLong(offset)
    if (value < 0) fail(s"$label exceeds the supported signed 64-bit range")
    value
  }

  private def fail(message: String): Nothing = throw new IllegalArgumentException(message)
}

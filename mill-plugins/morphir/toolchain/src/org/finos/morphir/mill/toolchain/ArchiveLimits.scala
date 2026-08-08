package org.finos.morphir.mill.toolchain

/** Resource limits applied while extracting a verified archive. Defaults accommodate Node distributions. */
final case class ArchiveLimits(
    maxEntries: Long = 100000L,
    maxEntryUncompressedBytes: StorageSize = storageSize"2 GiB",
    maxTotalUncompressedBytes: StorageSize = storageSize"8 GiB",
    maxCompressionRatio: Double = 200.0,
    maxSymlinkTargetBytes: StorageSize = storageSize"4 KiB",
    maxCompressedArchiveBytes: StorageSize = storageSize"1 GiB",
    maxCentralDirectoryBytes: StorageSize = storageSize"128 MiB",
    maxMetadataEntryBytes: StorageSize = storageSize"1 MiB",
    maxArchiveMetadataBytes: StorageSize = storageSize"16 MiB",
    maxConsecutiveMetadataEntries: Long = 32L,
    maxGzipHeaderBytes: StorageSize = storageSize"64 KiB"
) {
  require(maxEntries > 0, "maxEntries must be positive")
  require(maxEntryUncompressedBytes.toBytes > 0, "maxEntryUncompressedBytes must be positive")
  require(maxTotalUncompressedBytes.toBytes > 0, "maxTotalUncompressedBytes must be positive")
  require(maxCompressionRatio.isFinite && maxCompressionRatio >= 1.0, "maxCompressionRatio must be finite and >= 1")
  require(maxSymlinkTargetBytes.toBytes > 0, "maxSymlinkTargetBytes must be positive")
  require(maxCompressedArchiveBytes.toBytes > 0, "maxCompressedArchiveBytes must be positive")
  require(maxCentralDirectoryBytes.toBytes > 0, "maxCentralDirectoryBytes must be positive")
  require(maxMetadataEntryBytes.toBytes > 0, "maxMetadataEntryBytes must be positive")
  require(maxArchiveMetadataBytes.toBytes > 0, "maxArchiveMetadataBytes must be positive")
  require(maxConsecutiveMetadataEntries > 0, "maxConsecutiveMetadataEntries must be positive")
  require(maxGzipHeaderBytes.toBytes > 0, "maxGzipHeaderBytes must be positive")
}

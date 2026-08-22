package org.finos.morphir.mill.publish.desktop

import org.finos.morphir.mill.publish.Sha256Digest

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import scala.util.Using

/**
 * One file inside a canonicalized release directory, as data.
 *
 * `sha256` is recomputed from the bytes actually on disk — never copied from a sidecar or a prior manifest — so a
 * [[ReleaseCheck]] comparing it against a sidecar's recorded digest genuinely re-verifies the bytes, rather than
 * comparing a value against itself.
 */
final case class ReleaseFile(
    name: String,
    size: Long,
    sha256: String,
    headBytes: IndexedSeq[Byte],
    text: Option[String]
)

/**
 * A canonicalized desktop release directory, as data: the version being published, the platforms it is expected to
 * cover, and every file present.
 *
 * [[ReleaseCheck]]s only ever see a `ReleaseSnapshot` — never the filesystem — which is what keeps them pure and cheap
 * to unit test. [[ReleaseSnapshot.read]] is the one place that touches disk.
 */
final case class ReleaseSnapshot(version: String, platforms: Seq[DesktopPlatform], files: Seq[ReleaseFile]) {
  def file(name: String): Option[ReleaseFile] = files.find(_.name == name)
}

object ReleaseSnapshot {

  /** Bytes captured per file for magic-number checks — enough for every archive signature this package checks. */
  private val HeadBytesLength = 8

  /** Files at or under this size are decoded as text; sidecars, `checksums.txt` and `.asc` are all tiny. */
  private val TextSizeLimit = 64 * 1024L

  /** Reads a canonicalized release directory into a [[ReleaseSnapshot]]. The one IO boundary in this package. */
  def read(
      releaseDir: os.Path,
      version: String,
      platforms: Seq[DesktopPlatform] = DesktopPlatform.values.toSeq
  ): ReleaseSnapshot = {
    val files = os.list(releaseDir).filter(os.isFile).sortBy(_.last).map { path =>
      val size = os.size(path)
      ReleaseFile(
        name = path.last,
        size = size,
        sha256 = Sha256Digest.ofFile(path),
        headBytes = readHead(path, HeadBytesLength),
        text = if (size <= TextSizeLimit) decodeUtf8(os.read.bytes(path)) else None
      )
    }
    ReleaseSnapshot(version, platforms, files)
  }

  private def readHead(path: os.Path, length: Int): IndexedSeq[Byte] = {
    val buffer = new Array[Byte](length)
    Using.resource(java.nio.file.Files.newInputStream(path.toNIO)) { input =>
      val read = input.readNBytes(buffer, 0, length)
      buffer.take(read).toIndexedSeq
    }
  }

  /** Strict decode: a genuinely binary file (an archive, an installer) comes back `None`, not mojibake. */
  private def decodeUtf8(bytes: Array[Byte]): Option[String] = {
    val decoder = StandardCharsets.UTF_8.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    try Some(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch { case _: CharacterCodingException => None }
  }
}

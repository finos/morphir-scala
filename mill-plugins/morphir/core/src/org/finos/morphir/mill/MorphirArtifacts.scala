package org.finos.morphir.mill

import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, LinkOption, OpenOption, StandardOpenOption}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

import mill.PathRef
import upickle.default.ReadWriter

final case class MorphirIrArtifact(moduleId: ModuleId, path: PathRef, sha256: String) derives ReadWriter {
  MorphirArtifactIdentity.requireSha256(sha256)

  /** Compatibility spelling retained for consumers of the original metabuild API. */
  def irFilePath: PathRef = path
}

object MorphirIrArtifact {
  def fromFile(moduleId: ModuleId, path: PathRef): MorphirIrArtifact =
    MorphirIrArtifact(moduleId, path, MorphirArtifactIdentity.sha256(path.path))
}

final case class MorphirDependencyArtifact(moduleId: ModuleId, irFilePath: PathRef, sha256: String)
    derives ReadWriter {
  MorphirArtifactIdentity.requireSha256(sha256)
}

object MorphirDependencyArtifact {
  def fromArtifact(artifact: MorphirIrArtifact): MorphirDependencyArtifact =
    MorphirDependencyArtifact(artifact.moduleId, artifact.path, artifact.sha256)
}

object MorphirArtifactIdentity {
  private val FullSha256 = "[0-9a-f]{64}".r

  def sha256(path: os.Path): String = sha256(path, () => ())

  private[mill] def sha256(path: os.Path, beforeOpen: () => Unit): String = {
    val nio    = path.toNIO
    val before = readAttributes(nio, path)
    if (before.isSymbolicLink || !before.isRegularFile || Files.isSymbolicLink(nio))
      throw new IllegalArgumentException(s"Morphir IR artifact is not a regular non-symbolic-link file: $path")

    beforeOpen()
    val options = Set[OpenOption](StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS).asJava
    val input   = try Files.newByteChannel(nio, options)
    catch {
      case error: java.io.IOException =>
        throw new IllegalArgumentException(s"Morphir IR artifact changed before it could be opened: $path", error)
    }
    val digest    = MessageDigest.getInstance("SHA-256")
    var bytesRead = 0L
    try {
      val buffer = ByteBuffer.allocate(8192)
      var read   = input.read(buffer)
      while (read >= 0) {
        if (read > 0) {
          bytesRead += read
          buffer.flip()
          digest.update(buffer)
          buffer.clear()
        }
        read = input.read(buffer)
      }
    } finally input.close()

    val after       = readAttributes(nio, path)
    val sameFileKey = before.fileKey() == null || after.fileKey() == null || before.fileKey() == after.fileKey()
    if (
      after.isSymbolicLink || !after.isRegularFile || Files.isSymbolicLink(nio) ||
      bytesRead != before.size() || after.size() != before.size() ||
      after.lastModifiedTime() != before.lastModifiedTime() || !sameFileKey
    ) throw new IllegalArgumentException(s"Morphir IR artifact changed while its identity was computed: $path")

    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
  }

  private def readAttributes(path: java.nio.file.Path, display: os.Path): BasicFileAttributes =
    try Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    catch {
      case error: java.io.IOException =>
        throw new IllegalArgumentException(
          s"Morphir IR artifact is not a readable non-symbolic-link file: $display",
          error
        )
    }

  private[mill] def requireSha256(value: String): Unit =
    if (!FullSha256.matches(value))
      throw new IllegalArgumentException(s"Morphir artifact identity must be a full lower-case SHA-256 digest: $value")
}

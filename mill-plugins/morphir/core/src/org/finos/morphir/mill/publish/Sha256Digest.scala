package org.finos.morphir.mill.publish

import java.nio.file.Files
import java.security.MessageDigest
import scala.util.Using

/** SHA-256 helpers for publish-side integrity files. */
object Sha256Digest {
  private val BufferSize = 64 * 1024

  def hex(bytes: Array[Byte]): String = render(MessageDigest.getInstance("SHA-256").digest(bytes))

  def ofFile(path: os.Path): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    Using.resource(Files.newInputStream(path.toNIO)) { input =>
      val buffer = new Array[Byte](BufferSize)
      var read   = input.read(buffer)
      while (read >= 0) {
        digest.update(buffer, 0, read)
        read = input.read(buffer)
      }
    }
    render(digest.digest())
  }

  private def render(bytes: Array[Byte]): String =
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
}

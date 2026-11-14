package morphir.spikes.wasmhost.tools

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import run.endive.wabt.Wat2Wasm

/** Builds original spike WAT sources into derived binaries; no generated binary is committed. */
object Fixtures:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "fixtures requires source and output directories")
    val source = Path.of(args(0))
    val output = Path.of(args(1))
    Files.createDirectories(output)
    val files = Files.list(source)
    try
      files.filter(_.toString.endsWith(".wat")).sorted().forEach { path =>
        val bytes = Wat2Wasm.parse(Files.readString(path))
        val name  = path.getFileName.toString.stripSuffix(".wat") + ".wasm"
        Files.write(output.resolve(name), bytes)
        val sha = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
        println(s"$name ${bytes.length} $sha")
      }
    finally files.close()

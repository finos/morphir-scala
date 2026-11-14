package morphir.spikes.wasmhost.tools

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.Path
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarOutputStream}
import java.util.zip.GZIPOutputStream

/** Packages only runtime inputs; compiler, WAT tooling and unused native archives are excluded. */
object Distribution:
  private def absolute(path: String): os.Path = os.Path(Path.of(path).toRealPath())
  def hash(file: os.Path): String             =
    val digest = MessageDigest.getInstance("SHA-256")
    val stream = java.security.DigestInputStream(java.nio.file.Files.newInputStream(file.toNIO), digest)
    try
      val _ = stream.transferTo(java.io.OutputStream.nullOutputStream())
      digest.digest().map(b => f"${b & 255}%02x").mkString
    finally stream.close()

  def files(root: os.Path): Seq[os.Path] = os.walk(root).filter(path => os.isFile(path) && !os.isLink(path)).sorted

  private def jarDirectory(source: os.Path, target: os.Path): Unit =
    val zip = JarOutputStream(BufferedOutputStream(FileOutputStream(target.toIO)))
    zip.setLevel(6)
    try files(source).foreach { file =>
        val entry = JarEntry(file.relativeTo(source).toString)
        entry.setTime(0L)
        zip.putNextEntry(entry)
        val _ = java.nio.file.Files.copy(file.toNIO, zip)
        zip.closeEntry()
      }
    finally zip.close()

  def main(args: Array[String]): Unit =
    val configuration = ujson.read(os.read(os.Path(Path.of(args(0)).toAbsolutePath)))
    val output        = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val runtime = absolute(configuration("javaHome").str)
    Seq("javaHome", "nativeRuntime", "fixtures", "guests").foreach { key =>
      configuration(key) = absolute(configuration(key).str).toString
    }
    val runtimeBytes = files(runtime).map(os.size).sum
    val receipts     = configuration("configurations").arr.map { config =>
      val id   = config("id").str
      val root = output / id
      os.makeDir.all(root / "lib")
      val native = config("target").str == "native"
      if native then os.copy.over(absolute(config("executable").str), root / "host")
      else
        config("classpath").arr.filter(path => java.nio.file.Files.exists(Path.of(path.str))).zipWithIndex.foreach {
          (path, index) =>
            val source      = absolute(path.str)
            val destination = root / "lib" / s"$index.jar"
            if os.isDir(source) then jarDirectory(source, destination) else os.copy.over(source, destination)
        }
        os.write(
          root / "host",
          "#!/bin/sh\nAPP=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)\nexec \"${JAVA_HOME:?Set JAVA_HOME}/bin/java\" --enable-native-access=ALL-UNNAMED -cp \"$APP/lib/*\" " +
            config("mainClass").str + " \"$@\"\n"
        )
      os.perms.set(root / "host", "rwxr-xr-x")
      if config("backend").str == "extism-ffm" then
        val acquisition = os.Path(configuration("nativeRuntime").str)
        Seq("libextism.dylib", "LICENSE").foreach(name => os.copy.over(acquisition / name, root / name))
      val inventory = files(root).map(file =>
        ujson.Obj(
          "path"   -> file.relativeTo(root).toString,
          "bytes"  -> ujson.Num(os.size(file).toDouble),
          "sha256" -> hash(file)
        )
      )
      (os.walk(root) :+ root).foreach { path =>
        val _ = java.nio.file.Files.setLastModifiedTime(path.toNIO, java.nio.file.attribute.FileTime.fromMillis(0L))
      }
      val tar = output / s"$id.tar"
      os.proc(
        "tar",
        "--format=ustar",
        "--uid=0",
        "--gid=0",
        "--uname=root",
        "--gname=root",
        "-cf",
        tar,
        "-C",
        root,
        "."
      ).call(env = Map("COPYFILE_DISABLE" -> "1"))
      val archive    = output / s"$id.tar.gz"
      val compressed = GZIPOutputStream(BufferedOutputStream(FileOutputStream(archive.toIO)))
      try
        val _ = java.nio.file.Files.copy(tar.toNIO, compressed)
      finally compressed.close()
      os.remove(tar)
      config("command") = ujson.Arr((root / "host").toNIO.toRealPath().toString)
      if config("backend").str == "extism-ffm" then
        config("nativeLibrary") = (root / "libextism.dylib").toNIO.toRealPath().toString
      config("files") = ujson.Arr.from(inventory)
      config("executableOrLauncherBytes") = ujson.Num(os.size(root / "host").toDouble)
      config("installedBytes") = ujson.Num(inventory.map(_("bytes").num).sum)
      config("packageBytes") = ujson.Num(os.size(archive).toDouble)
      config("packageSha256") = hash(archive)
      config("package") = archive.toNIO.toRealPath().toString
      config("executableSha256") = hash(root / "host")
      config("debugSymbols") = "not produced separately"
      config("stripping") = "Native Image defaults, no explicit post-link strip"
      if native then
        val sourceLog = Path.of(config("executable").str).toRealPath().getParent.getParent.resolve("nativeImage.log")
        if java.nio.file.Files.isRegularFile(sourceLog) then
          val name = s"$id-build.log"
          os.copy.over(os.Path(sourceLog), output / name)
          config("nativeBuildLog") = name
          val log = os.read(output / name)
          config("nativeImageBuildTime") =
            "Finished generating .* in ([^\\n]+)".r.findFirstMatchIn(log).fold("unavailable")(_.group(1))
          config("nativeImageBuildPeakRss") =
            "Peak RSS: ([^|\\n]+)".r.findFirstMatchIn(log).fold("unavailable")(_.group(1).trim)
        else
          config("nativeImageBuildTime") = "unavailable"
          config("nativeImageBuildPeakRss") = "unavailable"
      config
    }
    configuration("configurations") = ujson.Arr.from(receipts)
    configuration("externalJavaRuntimeBytes") = ujson.Num(runtimeBytes.toDouble)
    configuration("compression") =
      "ustar + JDK GZIP default deflate level 6; normalized file timestamps and root ownership; executable modes retained; application files only"
    os.write(output / "configurations.json", configuration.render(indent = 2))

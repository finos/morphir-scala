package millbuild

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.io.InputStream
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.security.MessageDigest
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream, TarArchiveOutputStream}

/** Host-native release packaging for the Scala Elm MEP extension. */
object ExtensionRelease:
  enum Platform(val token: String):
    case MacAarch64   extends Platform("mac-aarch64")
    case MacAmd64     extends Platform("mac-amd64")
    case LinuxAmd64   extends Platform("linux-amd64")
    case LinuxAarch64 extends Platform("linux-aarch64")
    case WinAmd64     extends Platform("win-amd64")

  object Platform:
    val AllTokens: String = values.map(_.token).mkString(",")

    def fromToken(token: String): Either[String, Platform] =
      values.find(_.token == token).toRight(
        s"unknown extension platform '$token' (valid platforms: ${values.map(_.token).mkString(", ")})"
      )

    def fromHost(osName: String, osArch: String): Either[String, Platform] =
      val os   = osName.toLowerCase(java.util.Locale.ROOT)
      val arch = osArch.toLowerCase(java.util.Locale.ROOT)
      if os.startsWith("windows") && Set("aarch64", "arm64").contains(arch) then
        Left("Windows ARM64 has no GraalVM Native Image distribution for the Scala Elm MEP extension")
      else
        (os, arch) match
          case (name, value) if name.startsWith("windows") && Set("amd64", "x86_64", "x64").contains(value) =>
            Right(WinAmd64)
          case (name, value) if name.startsWith("linux") && Set("amd64", "x86_64", "x64").contains(value) =>
            Right(LinuxAmd64)
          case (name, value) if name.startsWith("linux") && Set("aarch64", "arm64").contains(value) =>
            Right(LinuxAarch64)
          case (name, value) if name.startsWith("mac") && Set("amd64", "x86_64", "x64").contains(value) =>
            Right(MacAmd64)
          case (name, value) if name.startsWith("mac") && Set("aarch64", "arm64").contains(value) =>
            Right(MacAarch64)
          case _ => Left(s"unsupported extension native-image host: $osName / $osArch")

  def mepAssetName(platform: Platform, version: String): String =
    s"morphir-scala-elm-${platform.token}-${safeVersion(version)}" +
      (if platform == Platform.WinAmd64 then ".exe" else "")

  def nativeTransportName(platform: Platform, version: String): String =
    s"morphir-native-transport-${platform.token}-${safeVersion(version)}.tar"

  def packageMepNative(platform: Platform, version: String, executable: os.Path, releaseDir: os.Path): os.Path =
    require(os.isFile(executable), s"Elm MEP native executable does not exist: $executable")
    os.makeDir.all(releaseDir)
    val asset = releaseDir / mepAssetName(platform, version)
    os.copy.over(executable, asset)
    if platform != Platform.WinAmd64 then
      require(asset.toIO.setExecutable(true, false), s"could not make Elm MEP release asset executable: $asset")
    writeSidecar(asset)
    asset

  def packageNativeTransport(platform: Platform, version: String, releaseDir: os.Path, transportDir: os.Path): os.Path =
    val names   = nativeReleaseNames(platform, version)
    val missing = names.filterNot(name => os.isFile(releaseDir / name))
    require(missing.isEmpty, s"native transport is missing release files: ${missing.mkString(", ")}")
    os.makeDir.all(transportDir)
    val transport = transportDir / nativeTransportName(platform, version)
    val output    = TarArchiveOutputStream(BufferedOutputStream(Files.newOutputStream(transport.toNIO)))
    output.setLongFileMode(TarArchiveOutputStream.LONGFILE_POSIX)
    try names.foreach { name =>
        val source = releaseDir / name
        val entry  = TarArchiveEntry(name)
        entry.setSize(os.size(source))
        entry.setMode(if name == mepAssetName(platform, version) && platform != Platform.WinAmd64 then 0x1ed else 0x1a4)
        entry.setModTime(0L)
        entry.setUserId(0)
        entry.setGroupId(0)
        entry.setUserName("")
        entry.setGroupName("")
        output.putArchiveEntry(entry)
        Files.copy(source.toNIO, output)
        output.closeArchiveEntry()
      }
    finally output.close()
    transport

  def extractNativeTransport(
      platform: Platform,
      version: String,
      transport: os.Path,
      releaseDir: os.Path
  ): Either[Seq[String], Seq[String]] =
    if !os.isFile(transport) then Left(Seq(s"missing native transport: ${transport.last}"))
    else
      val expected = nativeReleaseNames(platform, version)
      os.makeDir.all(releaseDir)
      val input = TarArchiveInputStream(BufferedInputStream(Files.newInputStream(transport.toNIO)))
      @annotation.tailrec
      def loop(found: Set[String], problems: Vector[String]): Either[Seq[String], Seq[String]] =
        val entry = input.getNextEntry
        if entry == null then
          val allProblems = problems ++ expected.filterNot(found).map(name => s"missing native transport entry: $name")
          if allProblems.nonEmpty then Left(allProblems) else Right(expected)
        else
          val name = entry.getName
          if entry.isDirectory || !expected.contains(name) || name.contains('/') || name.contains('\\') then
            loop(found, problems :+ s"unexpected native transport entry: $name")
          else if found.contains(name) then loop(found, problems :+ s"duplicate native transport entry: $name")
          else
            val destination = releaseDir / name
            Files.copy(input, destination.toNIO, StandardCopyOption.REPLACE_EXISTING)
            if name == mepAssetName(platform, version) && platform != Platform.WinAmd64 &&
              !destination.toIO.setExecutable(true, false)
            then loop(found + name, problems :+ s"could not restore executable mode: $name")
            else loop(found + name, problems)
      try loop(Set.empty, Vector.empty)
      finally input.close()

  def verifyAndWriteChecksums(
      releaseDir: os.Path,
      version: String,
      platforms: Seq[Platform],
      requireExecutable: Boolean = false
  ): Either[Seq[String], Seq[String]] =
    if platforms.isEmpty then Left(Seq("no extension release assets requested"))
    else
      val assetNames    = platforms.map(mepAssetName(_, version))
      val unixMepNames  = platforms.filterNot(_ == Platform.WinAmd64).map(mepAssetName(_, version)).toSet
      val expectedFiles = assetNames.flatMap(name => Seq(name, s"$name.sha256")).toSet + "checksums.txt"
      val presentFiles  =
        if os.isDir(releaseDir) then os.list(releaseDir).filter(os.isFile).map(_.last).toSet else Set.empty[String]
      val unexpected = (presentFiles -- expectedFiles).toSeq.sorted.map(name => s"unexpected release file: $name")
      val problems   = assetNames.flatMap { name =>
        val asset   = releaseDir / name
        val sidecar = releaseDir / s"$name.sha256"
        if !os.isFile(asset) then Seq(s"missing release asset: $name")
        else if os.size(asset) == 0 then Seq(s"empty release asset: $name")
        else if requireExecutable && unixMepNames.contains(name) && !Files.isExecutable(asset.toNIO) then
          Seq(s"non-executable release asset: $name")
        else if !os.isFile(sidecar) then Seq(s"missing checksum sidecar: $name.sha256")
        else
          val expected = s"${sha256(asset)}  $name\n"
          val actual   = os.read(sidecar)
          if actual == expected then Seq.empty else Seq(s"digest mismatch: $name")
      } ++ unexpected

      if problems.nonEmpty then Left(problems)
      else
        val lines = assetNames.sorted.map(name => os.read(releaseDir / s"$name.sha256").trim)
        os.write.over(releaseDir / "checksums.txt", lines.mkString("", "\n", "\n"))
        Right(assetNames)

  private def nativeReleaseNames(platform: Platform, version: String): Seq[String] =
    val mep = mepAssetName(platform, version)
    Seq(mep, s"$mep.sha256")

  private def safeVersion(version: String): String =
    require(version.nonEmpty, "extension release version must not be empty")
    require(version.matches("[A-Za-z0-9][A-Za-z0-9._+-]*"), s"unsafe extension release version: '$version'")
    version

  private def writeSidecar(asset: os.Path): Unit =
    val sidecar = os.Path(asset.toString + ".sha256")
    os.write.over(sidecar, s"${sha256(asset)}  ${asset.last}\n")

  private def sha256(path: os.Path): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val input  = Files.newInputStream(path.toNIO)
    val buffer = Array.ofDim[Byte](64 * 1024)
    try updateDigest(input, buffer, digest)
    finally input.close()
    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString

  @annotation.tailrec
  private def updateDigest(input: InputStream, buffer: Array[Byte], digest: MessageDigest): Unit =
    val read = input.read(buffer)
    if read >= 0 then
      if read > 0 then digest.update(buffer, 0, read)
      updateDigest(input, buffer, digest)

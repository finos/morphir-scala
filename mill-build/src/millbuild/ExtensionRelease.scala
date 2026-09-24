package millbuild

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.io.InputStream
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.security.MessageDigest
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream, TarArchiveOutputStream}
import morphir.langkit.elm.compiler.mep.BuildExtensionDefinition as ExtensionDefinition

/** Host-native release packaging for the Scala Elm MEP extension. */
object ExtensionRelease:
  enum Platform(val token: String, val triple: String):
    case MacAarch64   extends Platform("mac-aarch64", "aarch64-apple-darwin")
    case MacAmd64     extends Platform("mac-amd64", "x86_64-apple-darwin")
    case LinuxAmd64   extends Platform("linux-amd64", "x86_64-unknown-linux-gnu")
    case LinuxAarch64 extends Platform("linux-aarch64", "aarch64-unknown-linux-gnu")
    case WinAmd64     extends Platform("win-amd64", "x86_64-pc-windows-msvc")

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
    s"${ExtensionDefinition.Id}-${platform.token}-${safeVersion(version)}" +
      (if platform == Platform.WinAmd64 then ".exe" else "")

  def bundleAssetName(version: String): String =
    s"${ExtensionDefinition.Id}-${safeVersion(version)}.bundle.release.json"

  /** Keep release.json in the local bundle, but upload it under an extension-specific asset name. */
  def githubReleaseAssets(releaseDir: os.Path, version: String, stagingDir: os.Path): Seq[os.Path] =
    val descriptor = releaseDir / "release.json"
    require(os.isFile(descriptor), "missing release.json; run ci.extensions.verify first")
    val staged = stagingDir / bundleAssetName(version)
    os.copy.over(descriptor, staged, createFolders = true)
    (os.list(releaseDir).filter(os.isFile).filterNot(path =>
      path.last == "release.json" || path.last == bundleAssetName(version)
    ).toSeq :+ staged).sortBy(_.last)

  private[millbuild] def bundleClaims(version: String): ujson.Value =
    ujson.Obj(
      "claimsVersion"    -> ExtensionDefinition.ClaimsVersion,
      "protocolVersions" -> ujson.Arr(ExtensionDefinition.ProtocolVersion),
      "extension"        -> ujson.Obj(
        "id"      -> ExtensionDefinition.Id,
        "name"    -> ExtensionDefinition.Name,
        "version" -> version,
        "types"   -> ujson.Arr.from(ExtensionDefinition.Types)
      ),
      "capabilities" -> ujson.read(ExtensionDefinition.CapabilitiesJson)
    )

  private def bundleDescriptor(releaseDir: os.Path, version: String, platforms: Seq[Platform]): ujson.Value =
    val claims = bundleClaims(version)
    ujson.Obj(
      "schemaVersion"       -> "2.0.0-draft.2",
      "extensionId"         -> ExtensionDefinition.Id,
      "shortId"             -> ExtensionDefinition.ShortId,
      "version"             -> version,
      "platformDifferences" -> "none",
      "artifacts"           -> ujson.Arr.from(platforms.sortBy(_.triple).map { platform =>
        val name = mepAssetName(platform, version)
        ujson.Obj(
          "platform" -> platform.triple,
          "runtime"  -> "process",
          "filename" -> name,
          "sha256"   -> sha256(releaseDir / name),
          "claims"   -> claims
        )
      })
    )

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
    else if platforms.distinct.size != platforms.size then Left(Seq("duplicate extension release platforms"))
    else
      val assetNames      = platforms.map(mepAssetName(_, version))
      val unixMepNames    = platforms.filterNot(_ == Platform.WinAmd64).map(mepAssetName(_, version)).toSet
      val descriptorNames = Seq("release.json", bundleAssetName(version))
      val expectedFiles   = assetNames.flatMap(name => Seq(name, s"$name.sha256")).toSet ++ descriptorNames +
        "checksums.txt"
      val presentFiles =
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
        val descriptor = bundleDescriptor(releaseDir, version, platforms)
        // Local release.json is generated output; a downloaded, named descriptor is an input to verify.
        val descriptorProblems = Seq(bundleAssetName(version)).filter(presentFiles).flatMap { name =>
          val matches = scala.util.Try(ujson.read(os.read(releaseDir / name)) == descriptor).getOrElse(false)
          if matches then Seq.empty else Seq(s"bundle descriptor mismatch: $name")
        }
        if descriptorProblems.nonEmpty then Left(descriptorProblems)
        else
          val lines = assetNames.sorted.map(name => os.read(releaseDir / s"$name.sha256").trim)
          os.write.over(releaseDir / "checksums.txt", lines.mkString("", "\n", "\n"))
          os.write.over(releaseDir / "release.json", ujson.write(descriptor, indent = 2) + "\n")
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

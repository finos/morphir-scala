package millbuild.crossplatform
import mill.*
import mill.api.DynamicModule
import mill.scalajslib.*
import mill.scalalib.*
import mill.scalanativelib.*
import _root_.millbuild.CommonScalaModule

trait CrossPlatformScalaModule extends PlatformScalaModule with CommonScalaModule {
  // Since we only target Scala 3, include src-3 directories
  // Platform suffixes are handled separately (jvm/js/native folders)
  def crossPlatformSourceSuffixes(srcFolderName: String) =
    for {
      versionSuffix  <- Seq("", "3")  // Include both src and src-3
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => srcFolderName
      case ("", suffix) => s"${srcFolderName}-$suffix"
      case (ver, "")    => s"${srcFolderName}-$ver"
      case (ver, pf)    => s"${srcFolderName}-${ver}-${pf}"
    }

  def crossPlatformRelativeSourcePaths(srcFolderName: String): Seq[os.RelPath] =
    for {
      versionSuffix  <- Seq("", "3")  // Include both src and src-3
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => os.rel / srcFolderName
      case ("", suffix) => os.rel / suffix / srcFolderName
      case (ver, "")    => os.rel / s"${srcFolderName}-$ver"
      case (ver, pf)    => os.rel / pf / s"${srcFolderName}-${ver}"
    }

  def platformFolderMode: Platform.FolderMode = Platform.FolderMode.UseNesting

  def crossPlatformSourcePaths: Seq[PathRef] =
    platformFolderMode match {
      case Platform.FolderMode.UseSuffix =>
        crossPlatformSourceSuffixes("src").map(suffix => PathRef(moduleDir / suffix))
      case Platform.FolderMode.UseNesting =>
        crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(moduleDir / subPath))
      case Platform.FolderMode.UseBoth =>
        (crossPlatformSourceSuffixes("src").map(suffix => PathRef(moduleDir / suffix)) ++
          crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(moduleDir / subPath))).distinct
    }

  def platformSpecificModuleDeps: Seq[CrossPlatform]         = Seq.empty
  def platformSpecificCompiledModuleDeps: Seq[CrossPlatform] = Seq.empty

  override def moduleDeps = super.moduleDeps ++ platformSpecificModuleDeps.flatMap {
    case p => p.childPlatformModules(platform)
  }
  override def compileModuleDeps =
    super.compileModuleDeps ++ platformSpecificCompiledModuleDeps.flatMap(_.childPlatformModules(platform))
  def platform: Platform
  def knownPlatforms: T[Seq[Platform]] = Task { Platform.all.toSeq }

  override def sources: T[Seq[PathRef]] = Task.Sources(crossPlatformSourcePaths.map(_.path)*)
}

package millbuild.crossplatform
import mill.*
import mill.api.DynamicModule
import mill.scalajslib.*
import mill.scalalib.*
import mill.scalanativelib.*
import _root_.millbuild.CommonCrossScalaModule

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule with CommonCrossScalaModule {
  def crossPlatformSourceSuffixes(srcFolderName: String) =
    for {
      versionSuffix  <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => srcFolderName
      case ("", suffix) => s"${srcFolderName}-$suffix"
      case (suffix, "") => s"${srcFolderName}-$suffix"
      case (vs, ps)     => s"${srcFolderName}-${vs}-${ps}"
    }

  def crossPlatformRelativeSourcePaths(srcFolderName: String): Seq[os.RelPath] =
    for {
      versionSuffix  <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => os.rel / srcFolderName
      case ("", suffix) => os.rel / suffix / srcFolderName
      case (suffix, "") => os.rel / s"${srcFolderName}-$suffix"
      case (vs, ps)     => os.rel / ps / s"${srcFolderName}-${vs}"
    }

  def platformFolderMode: Platform.FolderMode = Platform.FolderMode.UseNesting

  def crossPlatformSourcePaths: Seq[os.Path] =
    platformFolderMode match {
      case Platform.FolderMode.UseSuffix =>
        crossPlatformSourceSuffixes("src").map(suffix => moduleDir / suffix)
      case Platform.FolderMode.UseNesting =>
        crossPlatformRelativeSourcePaths("src").map(subPath => moduleDir / subPath)
      case Platform.FolderMode.UseBoth =>
        (crossPlatformSourceSuffixes("src").map(suffix => moduleDir / suffix) ++
          crossPlatformRelativeSourcePaths("src").map(subPath => moduleDir / subPath)).distinct
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

  override def sources: T[Seq[PathRef]] = Task.Sources(crossPlatformSourcePaths*)
}

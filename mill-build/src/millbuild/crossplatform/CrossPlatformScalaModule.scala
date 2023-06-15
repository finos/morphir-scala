package millbuild.crossplatform
import mill._
import mill.scalalib._ 

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule {
  def crossPlatformSourceSuffixes = 
    for {
      versionSuffix <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield {
        (versionSuffix, platformSuffix) match {
          case ("","") => "src"
          case ("", suffix) => s"src-$suffix"
          case (suffix, "") => s"src-$suffix"
          case (vs, ps) => s"src-${vs}-${ps}"
        }
    }

  def crossPlatformRelativeSourcePaths:Seq[os.RelPath] = 
    for {
      versionSuffix <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield {
        (versionSuffix, platformSuffix) match {
          case ("","") => os.rel / "src"
          case ("", suffix) => os.rel / suffix / "src"
          case (suffix, "") => os.rel / s"src-$suffix"
          case (vs, ps) => os.rel / ps /  s"src-${vs}"
        }
    }

  def crossPlatformSources:T[Seq[PathRef]] = T.sources {
    platformFolderMode() match {
      case Platform.FolderMode.UseSuffix => 
        crossPlatformSourceSuffixes.map(suffix => PathRef(millSourcePath / suffix) )
      case Platform.FolderMode.UseNesting =>
        crossPlatformRelativeSourcePaths.map(subPath => PathRef(millSourcePath / subPath))
      case Platform.FolderMode.UseBoth =>
        (crossPlatformSourceSuffixes.map(suffix => PathRef(millSourcePath / suffix) ) ++ 
          crossPlatformRelativeSourcePaths.map(subPath => PathRef(millSourcePath / subPath))).distinct
    }
  }

  def platformFolderMode:T[Platform.FolderMode] = T {Platform.FolderMode.UseNesting}
  def platform:Platform
  def platforms:T[Seq[Platform]] = T { Platform.all.toSeq }    

  def sources = T.sources { 
   crossPlatformSources()
  }
}

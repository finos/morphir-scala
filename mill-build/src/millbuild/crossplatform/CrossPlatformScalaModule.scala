package millbuild.crossplatform
import mill._
import mill.scalalib._ 

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule {
  def crossPlatformSourceSuffixes(srcFolderName:String) = 
    for {
      versionSuffix <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield {
        (versionSuffix, platformSuffix) match {
          case ("","") => srcFolderName
          case ("", suffix) => s"${srcFolderName}-$suffix"
          case (suffix, "") => s"${srcFolderName}-$suffix"
          case (vs, ps) => s"${srcFolderName}-${vs}-${ps}"
        }
    }

  def crossPlatformRelativeSourcePaths(srcFolderName:String):Seq[os.RelPath] = 
    for {
      versionSuffix <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield {
        (versionSuffix, platformSuffix) match {
          case ("","") => os.rel / srcFolderName
          case ("", suffix) => os.rel / suffix / srcFolderName
          case (suffix, "") => os.rel / s"${srcFolderName}-$suffix"
          case (vs, ps) => os.rel / ps /  s"${srcFolderName}-${vs}"
        }
    }

  def crossPlatformSources:T[Seq[PathRef]] = T.sources {    
    platformFolderMode() match {
      case Platform.FolderMode.UseSuffix => 
        crossPlatformSourceSuffixes("src").map(suffix => PathRef(millSourcePath / suffix) )
      case Platform.FolderMode.UseNesting =>
        crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(millSourcePath / subPath))
      case Platform.FolderMode.UseBoth =>
        (crossPlatformSourceSuffixes("src").map(suffix => PathRef(millSourcePath / suffix) ) ++ 
          crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(millSourcePath / subPath))).distinct
    }
  }

  def platformFolderMode:T[Platform.FolderMode] = T {Platform.FolderMode.UseNesting}
  def platform:Platform
  def platforms:T[Seq[Platform]] = T { Platform.all.toSeq }    

  def sources = T.sources { 
   crossPlatformSources()
  }
}

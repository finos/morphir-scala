package millbuild.crossplatform
import mill._
import mill.scalalib._ 

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule {
  def crossPlatformSuffixesT[Seq[String]] = T{
    scalaVersionDirectoryNames
  }
  def platform:Platform
  def platforms:T[Seq[Platform]] = T { Platform.all.toSeq }    
  //TODO: Use a flag to determine if source folder type should be src-3.3.0-js-jvm style or js-jvm/src-3.3.0 style

  def sources = T.sources { 
    (super.sources() ++ (for {
      source <- super.sources()
      suffix <- platform.suffixes        
      if !source.path.last.endsWith(platform.name)
    } yield PathRef(source.path/ _root_.os.up / s"${source.path.last}-${suffix}" ))).distinct
  }
}

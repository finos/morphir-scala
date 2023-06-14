package millbuild.crossplatform
import mill._
import mill.scalalib._ 

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule {
  def platform:Platform
  def platforms:T[Seq[Platform]] = T { Platform.all.toSeq }    
  def sources = T.sources { 
    (super.sources() ++ (for {
      source <- super.sources()
      suffix <- platform.suffixes        
      if !source.path.last.endsWith(platform.name)
    } yield PathRef(source.path/ _root_.os.up / s"${source.path.last}-${suffix}" ))).distinct
  }
}

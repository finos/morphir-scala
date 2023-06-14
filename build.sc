import $meta._
import $file.project.deps, deps.{Deps, ScalaVersions, Versions => Vers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import millbuild.crossplatform.{Platform}
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._


object morphir extends Cross[MorphirModule](ScalaVersions.all)
trait MorphirModule extends Cross.Module[String] {
  val workspaceDir = build.millSourcePath

  trait Shared extends CrossScalaModule with CrossValue with PlatformScalaModule {
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

  trait SharedJVM extends Shared {
    def platform = Platform.JVM
  }

  trait SharedJS extends Shared with ScalaJSModule {
    def platform = Platform.JS
    def scalaJSVersion = ScalaVersions.scalaJSVersion 
  }

  trait SharedNative extends Shared with ScalaNativeModule {
    def platform = Platform.Native
    def scalaNativeVersion = ScalaVersions.scalaNativeVersion
  }

  object datamodel extends Module {
    object jvm extends SharedJVM
    object js extends SharedJS
    object native extends SharedNative
  }

  object testing extends Module {
    object jvm extends SharedJVM
    object js extends SharedJS
    object native extends SharedNative

    object munit extends Module {
      object jvm extends SharedJVM
      object js extends SharedJS
      object native extends SharedNative
    }

    object zio extends Module {
      object jvm extends SharedJVM
      object js extends SharedJS
      object native extends SharedNative
    }
  }

}

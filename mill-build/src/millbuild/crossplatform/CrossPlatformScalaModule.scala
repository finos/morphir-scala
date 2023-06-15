package millbuild.crossplatform
import mill._
import mill.define.DynamicModule
import mill.main.BuildInfo
import mill.scalajslib._
import mill.scalalib._
import mill.scalanativelib._

trait CrossPlatform extends Module with DynamicModule { container =>
  def moduleDeps:Seq[CrossPlatform] = Seq.empty
  def compiledModuleDeps:Seq[CrossPlatform] = Seq.empty

  def enableJVM(module:Module):Boolean = true 
  def enableJS(module:Module):Boolean = true
  def enableNative(module:Module):Boolean = true

  private def enableModuleCondition(module:Module):Boolean = module match {
    case _:ScalaNativeModule => enableNative(module)
    case _:ScalaJSModule => enableJS(module)
    case _:ScalaModule => enableJVM(module)
    case c: Cross[_] =>
      enableModuleCondition(
        c.crossModules.head.asInstanceOf[Module]
      )
    case _ => true
  }

  override def millModuleDirectChildren: Seq[Module] =
    super.millModuleDirectChildren.filter(enableModuleCondition)

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

    // override def moduleDeps = super.moduleDeps ++
    //     container.moduleDeps.map(innerModule).asInstanceOf[Seq[this.type]]
    // override def compileModuleDeps = super.compileModuleDeps ++
    //     container.compileModuleDeps.map(innerModule).asInstanceOf[Seq[this.type]]

    //def innerModule(module:CrossPlatformModule) = module match {}

    def platformFolderMode:T[Platform.FolderMode] = T {Platform.FolderMode.UseNesting}
    def platform:Platform
    def platforms:T[Seq[Platform]] = T { Platform.all.toSeq }    

    def sources = T.sources { 
    crossPlatformSources()
    }
  }
}

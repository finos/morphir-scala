package millbuild.crossplatform

import mill.*
import mill.api.DynamicModule
import mill.scalajslib.*
import mill.scalalib.*
import mill.scalanativelib.*
import millbuild.settings._
import scala.language.reflectiveCalls
import millbuild.MyBuild

trait CrossPlatform extends Module with DynamicModule { self =>
  import DevMode._

  def buildSettings: BuildSettings           = MyBuild.cachedBuildSettings
  def moduleDeps: Seq[CrossPlatform]         = Seq.empty
  def compiledModuleDeps: Seq[CrossPlatform] = Seq.empty

  def enableJVM(module: Module): Boolean    = buildSettings.jvm.enable
  def enableJS(module: Module): Boolean     = !devMode && buildSettings.js.enable
  def enableNative(module: Module): Boolean = !devMode && buildSettings.native.enable

  private def enableModuleCondition(module: Module): Boolean = module match {
    case _: ScalaNativeModule => enableNative(module)
    case _: ScalaJSModule     => enableJS(module)
    case _: ScalaModule       => enableJVM(module)
    case c: Cross[_] =>
      enableModuleCondition(
        c.crossModules.head.asInstanceOf[Module]
      )
    case _ => true
  }

  final def originalModuleDirectChildren: Seq[Module] = super.moduleDirectChildren

  override def moduleDirectChildren: Seq[Module] =
    originalModuleDirectChildren.filter(enableModuleCondition)

  private type PlatformModule = JavaModule {
    def platform: Platform
  }

  object AsPlatformModule {
    def unapply(m: Module): Option[PlatformModule] = m match {
      case jm: JavaModule =>
        jm match {
          case (m: PlatformModule @unchecked) => Some(m)
          case _                              => None
        }
      case _ => None
    }
  }

  def childPlatformModules: Seq[PlatformModule] =
    moduleDirectChildren.collect { case (m: PlatformModule @unchecked) => m }

  def targetPlatforms: T[Seq[Platform]] = Task { childPlatformModules.map(_.platform) }

  /// Try and resolve to an actual platform specific module if it exists (not expecting multiple but in theory it is possible)
  def childPlatformModules(platform: Platform): Seq[PlatformModule] =
    childPlatformModules.filter {
      case m: CrossPlatformScalaModule => m.platform == platform
      case AsPlatformModule(m)         => m.platform == platform
    }
}

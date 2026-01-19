package millbuild.crossplatform

import mill.*
import mill.api.DynamicModule
import mill.scalajslib.*
import mill.scalalib.*
import mill.scalanativelib.*
import millbuild.config.PlatformConfig
import scala.reflect.Selectable.reflectiveSelectable

trait CrossPlatform extends Module with DynamicModule { self =>

  def moduleDeps: Seq[CrossPlatform]         = Seq.empty
  def compiledModuleDeps: Seq[CrossPlatform] = Seq.empty

  def enableJVM(module: Module): Boolean    = PlatformConfig.enableJVM
  def enableJS(module: Module): Boolean     = PlatformConfig.enableJS
  def enableNative(module: Module): Boolean = PlatformConfig.enableNative

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
        try {
          // Check if platform method exists by calling it via reflection
          val platformMethod = jm.getClass.getMethod("platform")
          Some(jm.asInstanceOf[PlatformModule])
        } catch {
          case _: NoSuchMethodException => None
        }
      case _ => None
    }
  }

  def childPlatformModules: Seq[PlatformModule] =
    moduleDirectChildren.collect { case AsPlatformModule(m) => m }

  def targetPlatforms: T[Seq[Platform]] = Task { childPlatformModules.map(_.platform) }

  /// Try and resolve to an actual platform specific module if it exists (not expecting multiple but in theory it is possible)
  def childPlatformModules(platform: Platform): Seq[PlatformModule] =
    childPlatformModules.filter(_.platform == platform)
}

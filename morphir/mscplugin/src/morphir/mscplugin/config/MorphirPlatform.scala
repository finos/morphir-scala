package morphir.mscplugin.config
import dotty.tools.dotc.core._
import Contexts._
import Symbols._

import dotty.tools.dotc.config.JavaPlatform
import morphir.mscplugin.MirDefinitions

object MorphirPlatform:
  def morphirPlatform(using Context):MorphirPlatform =
    ctx.platform.asInstanceOf[MorphirPlatform]

class MorphirPlatform()(using Context) extends JavaPlatform:
  val mirDefinitions: MirDefinitions = new MirDefinitions()

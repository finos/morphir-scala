package morphir.mscplugin.config

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.config.Settings.SettingGroup
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts._

object MorphirSettings extends SettingGroup:  
  val morphir:Setting[Boolean] = BooleanSetting("-morphir", "Compile in Morphir mode.", aliases=List("--morphir"))

class MorphirSettings(using Context):
  val morphir  = ctx.settings.BooleanSetting("-morphir", "Compile in Morphir mode.", aliases=List("--morphir"))
  

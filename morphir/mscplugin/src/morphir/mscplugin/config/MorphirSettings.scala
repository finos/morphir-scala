package morphir.mscplugin.config

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.config.Settings.SettingGroup

object MorphirSettings extends SettingGroup:  

  val morphir:Setting[Boolean] = BooleanSetting("-morphir", "Compile in Morphir mode.", aliases=List("--morphir"))

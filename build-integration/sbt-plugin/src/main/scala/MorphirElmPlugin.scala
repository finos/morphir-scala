package org.finos.morphir.build.sbt.plugin
import sbt._

object MorphirElmPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins      = plugins.JvmPlugin

  object autoImport {}

  import autoImport._

  override def projectSettings = super.projectSettings
}

package morphir.mscplugin

import dotty.tools.dotc.report
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{PickleQuotes, Staging}
import dotty.tools.dotc.core.Contexts.FreshContext

import morphir.mscplugin.config.MorphirSettings

class MorphirIRGenPhase extends PluginPhase:
  import tpd.*
  val phaseName = MorphirIRGenPhase.name
  override val runsAfter = Set(MorphirModuleSelectPhase.name)
  override val runsBefore = Set(PickleQuotes.name)

  override def initContext(ctx: FreshContext): Unit =
    ctx.setSetting(MorphirSettings.morphir, false)

object MorphirIRGenPhase:
  val name = "morphirIRGen"

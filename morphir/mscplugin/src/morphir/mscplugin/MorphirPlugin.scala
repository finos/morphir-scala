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

class MorphirPlugin extends StandardPlugin:
  val name: String = "morphir"
  override val description: String = "Morphir compiler plugin"
  def init(options:List[String]):List[PluginPhase] =
    (new MorphirModuleSelectPhase)::(new MorphirIRGenPhase)::Nil


class MorphirModuleSelectPhase extends PluginPhase:
  val phaseName = MorphirModuleSelectPhase.name
  override val runsAfter = Set(Staging.name)
  override val runsBefore = Set(MorphirIRGenPhase.name)

object MorphirModuleSelectPhase:
  val name = "morphirModuleSelect"
  
class MorphirIRGenPhase extends PluginPhase:
  import tpd.*
  val phaseName = MorphirIRGenPhase.name
  override val runsAfter = Set(MorphirModuleSelectPhase.name)
  override val runsBefore = Set(PickleQuotes.name)

object MorphirIRGenPhase:
  val name = "morphirIRGen"
  

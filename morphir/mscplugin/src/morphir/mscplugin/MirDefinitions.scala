package morphir.mscplugin

import dotty.tools.dotc.ast.tpd
import tpd._
import dotty.tools.dotc.core
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import dotty.tools.dotc.core.Phases

object MirDefinitions {
  private val cached = MirGenUtil.ContextCached(MirDefinitions())
  def get(using Context):MirDefinitions = cached.get
}

final class MirDefinitions()(using ctx:Context)

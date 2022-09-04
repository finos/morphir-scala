package morphir.mscplugin

import dotty.tools.dotc.core
import core.Contexts._
import morphir.mir

trait MirGenName(using Context):
  self: MirCodeGen =>

  def genName(sym:Symbol):mir.Name = ???
end MirGenName

object MirGenName

package morphir.mscplugin
import dotty.tools.dotc.ast.tpd
import tpd._
import dotty.tools.dotc.core
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._

import dotty.tools.dotc.core.Phases

object MirGenUtil {
  class ContextCached[T](init: Context ?=> T) {
    private var lastContext: Context = _
    private var cached: T = _

    def get(using Context): T = {
      if (lastContext != ctx) {
        cached = init
        lastContext = ctx
      }
      cached
    }
  }
}





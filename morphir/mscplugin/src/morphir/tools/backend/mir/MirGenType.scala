package morphir.tools.backend.mir

import dotty.tools.dotc.core._
import Names._
import Types._
import Contexts._
import Flags._
import Symbols._
import StdNames._
import MirGenUtil.ContextCached
import dotty.tools.dotc.transform.SymUtils._

trait MirGenType(using Context):
  extension (sym: Symbol)
    def isOpaque: Boolean = sym.is(Opaque)
    def isStaticModule: Boolean =
      sym.is(ModuleClass, butNot = Lifted)

    def isTraitOrInterface: Boolean =
      sym.is(Trait) || sym.isAllOf(JavaInterface)

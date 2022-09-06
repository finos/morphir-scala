package morphir.tools.backend.mir

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core
import core.Contexts._
import core.Symbols._
import core.Flags._
import core.StdNames._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import morphir.util.unreachable
import morphir.mir
import morphir.mir._
import scala.language.implicitConversions

trait MirGenName(using Context):
  self: MirCodeGen =>

  def genName(sym:Symbol):mir.Global =
    if (sym.isType) genTypeName(sym)
    else if(sym.is(Method)) genMethodName(sym)
    else genFieldName(sym)
  def genTypeName(sym:Symbol): mir.Global.Top = ???
  def genModuleName(sym:Symbol):mir.Global.Top = ???
  def genFieldName(sym:Symbol):mir.Global = ???

  def genMethodName(sym:Symbol):mir.Global = ???

end MirGenName

object MirGenName

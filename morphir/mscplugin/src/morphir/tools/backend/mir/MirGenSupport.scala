package morphir.tools.backend.mir
import scala.language.implicitConversions

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core
import core.Contexts._
import core.Symbols._
import core.Constants._
import core.Decorators.*
import core.StdNames._
import core.Flags._
import core.Phases._
import dotty.tools.dotc.transform.SymUtils._
import morphir.mir
import morphir.util.ScopedVar
import morphir.util.ScopedVar.{scoped, toValue}
import mir._
import mir.Module.{Definition => ModuleDefn}

import scala.collection.mutable

trait MirGenSupport(using Context):
  self: MirCodeGen =>
  import positionsConversions.fromSpan

  protected val generatedDefns = mutable.UnrolledBuffer.empty[Defn]
  def genClass(td: TypeDef)(using Context): Unit =
    val sym = td.symbol.asClass
    scoped() {
      if (sym.isStaticModule) genModule(td)
      else handleUnsupported(td)
    }

  def genModule(td: TypeDef): Unit =
    given pos: mir.Position = td.span
    val sys                 = td.symbol.asClass
    val attrs               = genClassAttrs(td)
    println(i"genModule for: ${td.symbol.name} @ position: ${pos.show}")

  private def genClassAttrs(td: TypeDef): mir.Attrs =
    val sym = td.symbol.asClass
    val annotationAttrs = sym.annotations.collect {
      case ann if ann.symbol == defnMir.ExternClass => Attr.Extern
    }
    Attrs.fromSeq(annotationAttrs)

  private def handleUnsupported(td:TypeDef):Unit = ()

end MirGenSupport

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
import mir.Module.{Definition => ModuleDefn}

import scala.collection.mutable


trait MirGenSupport(using Context):
  self: MirCodeGen =>
  
  protected val generatedModuleDefns = mutable.UnrolledBuffer.empty[ModuleDefn[Any,Any]]
  def genClass(td:TypeDef)(using Context):Unit = 
    val sym = td.symbol.asClass
    scoped(){
      if (sym.isStaticModule) genModule(td)
      else ()
    }

  def genModule(td:TypeDef):Unit = 
    println(i"genModule for: ${td.symbol.name}")

end MirGenSupport

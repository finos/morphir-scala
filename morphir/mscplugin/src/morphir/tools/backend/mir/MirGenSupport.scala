package morphir.tools.backend.mir
import scala.language.implicitConversions

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core
import core.Contexts._
import core.Symbols._
import core.Constants._
import core.StdNames._
import core.Flags._
import core.Phases._
import dotty.tools.dotc.transform.SymUtils._
import morphir.mir
import mir.Module.{Definition => ModuleDefn}

import scala.collection.mutable


trait MirGenSupport(using Context):
  self: MirCodeGen =>
  
  protected val generatedModuleDefns = mutable.UnrolledBuffer.empty[ModuleDefn[Any,Any]]

end MirGenSupport

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

  def genName(sym: Symbol): mir.Global =
    if (sym.isType) genTypeName(sym)
    else if (sym.is(Method)) genMethodName(sym)
    else genFieldName(sym)

  def genTypeName(sym: Symbol): mir.Global.Top = {
    val sym1 =
      if (sym.isAllOf(ModuleClass | JavaDefined) && sym.linkedClass.exists)
        sym.linkedClass
      else sym

    if (sym1 == defn.ObjectClass) Global.Top("java.lang.Object") // mir.Rt.Object.name.top
    else
      val id =
        val fullName = sym1.javaClassName
        MirGenName.MappedNames.getOrElse(fullName, fullName)
      Global.Top(id)
  }

  def genModuleName(sym: Symbol): mir.Global.Top = ???
  def genFieldName(sym: Symbol): mir.Global      = ???

  def genMethodName(sym: Symbol): mir.Global = ???

object MirGenName:
  private val MappedNames = Map(
    "java.lang._Class"                -> "java.lang.Class",
    "java.lang._Cloneable"            -> "java.lang.Cloneable",
    "java.lang._Comparable"           -> "java.lang.Comparable",
    "java.lang._Enum"                 -> "java.lang.Enum",
    "java.lang._NullPointerException" -> "java.lang.NullPointerException",
    "java.lang._Object"               -> "java.lang.Object",
    "java.lang._String"               -> "java.lang.String",
    "java.lang.annotation._Retention" -> "java.lang.annotation.Retention",
    "java.io._Serializable"           -> "java.io.Serializable"
  ).flatMap { case classEntry @ (nativeName, javaName) =>
    List(
      classEntry,
      (nativeName + "$", javaName + "$")
    )
  }

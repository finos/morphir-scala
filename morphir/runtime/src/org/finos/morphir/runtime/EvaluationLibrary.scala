package org.finos.morphir.runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.runtime.quick.{EvaluatorQuick, QuickMorphirRuntime, Store}
import org.finos.morphir.ir.Distribution.Distribution.Library
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Type.UType

case class EvaluationLibrary(runtime: MorphirRuntime[scala.Unit, UType], modulePrefix: Option[String]) {

  def deriveData(input: Any): Data =
    input match {
      case u: Unit             => Deriver.toData(u)
      case i: Int              => Deriver.toData(i)
      case s: String           => Deriver.toData(s)
      case (i: Int, s: String) => Data.Tuple(Deriver.toData(i), Deriver.toData(s))
      case other               => throw new Exception(s"Couldn't derive $other")
    }

  def runTestDDL(moduleName: String, functionName: String, input: Any): Any = {
    val fullName = modulePrefix match {
      case Some(prefix) => s"$prefix:$moduleName:$functionName"
      case None         => s"$moduleName:$functionName"
    }
    val derived = deriveData(input)
    val res     = runtime.evaluate(FQName.fromString(fullName), derived).runEither
    res match {
      case Right(res)  => res
      case Left(error) => throw error
    }
  }
}
object EvaluationLibrary extends EvaluationLibraryPlatformSpecific {}

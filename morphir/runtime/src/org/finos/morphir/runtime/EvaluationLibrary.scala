package org.finos.morphir.runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}
import org.finos.morphir.ir.Distribution.Distribution.Library

case class EvaluationLibrary(store: Store[Unit, Type.UType], modulePrefix: Option[String], distribution: Library) {
  def runTest(moduleName: String, functionName: String, input: Any): Any = {
    val fullName = modulePrefix match {
      case Some(prefix) => s"$prefix:$moduleName:$functionName"
      case None         => s"$moduleName:$functionName"
    }
    EvaluatorQuick.evalFunction(FQName.fromString(fullName), store, input)
  }

  def runTestDDL(moduleName: String, functionName: String, input: Any): Any = {
    val fullName = modulePrefix match {
      case Some(prefix) => s"$prefix:$moduleName:$functionName"
      case None         => s"$moduleName:$functionName"
    }
    EvaluatorQuick.evalFunctionToDDL(FQName.fromString(fullName), store, input, distribution)
  }
}
object EvaluationLibrary extends EvaluationLibraryPlatformSpecific {}

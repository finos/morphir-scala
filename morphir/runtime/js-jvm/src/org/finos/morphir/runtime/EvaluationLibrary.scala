package org.finos.morphir
package runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.MorphirIRFile

import scala.io.Source
import zio.json.*
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}

case class EvaluationLibrary(store: Store[Unit, Type.UType], modulePrefix: Option[String]) {
  def runTest(moduleName: String, functionName: String, input: Any): Any = {
    val fullName = modulePrefix match {
      case Some(prefix) => s"$prefix:$moduleName:$functionName"
      case None         => s"$moduleName:$functionName"
    }
    EvaluatorQuick.evalFunction(FQName.fromString(fullName), store, input)
  }
}
object EvaluationLibrary {
  def apply(fileName: String, prefix: Option[String] = None): EvaluationLibrary = {
    val text = Source
      .fromFile(fileName)
      .getLines()
      .mkString("\n")
    val morphirIRFile = text.fromJson[MorphirIRFile]
    val library = morphirIRFile
      .getOrElse(throw new Exception(morphirIRFile.toString))
      .distribution
      .asInstanceOf[Library]
    val store = Store.fromLibrary(library)
    EvaluationLibrary(store, prefix)
  }
  def apply(fileName: String, prefix: String): EvaluationLibrary = apply(fileName, Some(prefix))
}

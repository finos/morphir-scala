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
import zio.{test as _, *}
import zio.prelude.fx.*
import zio.test.*
import zio.test.Assertion.{equalTo, fails}
import zio.test.TestAspect.{ignore, tag}
import zio.json.*
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}

case class EvaluationLibrary(store : Store[Unit, Type.UType], modulePrefix : Option[String]){
  def runTest(moduleName : String, functionName : String, input : Any) : Any = {
    val fullName = modulePrefix match {
      case Some(prefix) => s"$prefix:$moduleName:$functionName"
      case None => s"$moduleName:$functionName"
    }
    EvaluatorQuick.evalFunction(FQName.fromString(fullName), store, input)
  }
}
object EvaluationLibrary{
  def apply(fileName : String, prefix : Option[String] = None) : EvaluationLibrary = {
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
  def apply(fileName : String, prefix : String) : EvaluationLibrary = apply(fileName, Some(prefix))
}

//object TestLibrary {
//  val fullName = s"./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json"
//  val text = Source
//    .fromFile(fullName)
//    .getLines()
//    .mkString("\n")
//  val morphirIRFile = text.fromJson[MorphirIRFile]
//  val library = morphirIRFile
//    .getOrElse(throw new Exception(morphirIRFile.toString))
//    .distribution
//    .asInstanceOf[Library]
//}
//
//object MappingLibrary{
//  val fullName = s"./examples/morphir-elm-projects/mapping-example/morphir-ir.json"
//  val text = Source
//    .fromFile(fullName)
//    .getLines()
//    .mkString("\n")
//  val morphirIRFile = text.fromJson[MorphirIRFile]
//  val library = morphirIRFile
//    .getOrElse(throw new Exception(morphirIRFile.toString))
//    .distribution
//    .asInstanceOf[Library]
//}
//
//object EvaluatorJsonHelpers {
//  def eval(moduleName: String, testName: String, lib: Library): Any = {
//    val store     = Store.fromLibrary(lib)
//    val defName   = FQName.fromString(s"Morphir.Examples.App:$moduleName:$testName")
//    val applyNode = V.apply(V.reference(defName), V.unit) :> T.unit // lies but I don't think we check?
//    EvaluatorQuick.evaluate[Unit, Type.UType](applyNode, store)
//  }
//
//  def runTest[T](fileName: String, moduleName: String, testName: String): T = {
//    val result = eval(moduleName, testName, TestLibrary.library).asInstanceOf[T]
//    result
//  }
//}
//
//object MappingDemoHelper{
//  def eval(moduleName: String, testName: String, lib: Library, value : Any): Any = {
//    val store = Store.fromLibrary(lib)
//    val defName = FQName.fromString(s"DynamicMapper:$moduleName:$testName")
//    val applyNode = V.apply(V.reference(defName), decompose(value)) :> T.unit // lies but I don't think we check?
//    EvaluatorQuick.evaluate[Unit, Type.UType](applyNode, store)
//  }
//
//  def runTest(moduleName: String, testName: String, value : Any): Any = {
//    val result = eval(moduleName, testName, MappingLibrary.library, value)
//    result
//  }
//}

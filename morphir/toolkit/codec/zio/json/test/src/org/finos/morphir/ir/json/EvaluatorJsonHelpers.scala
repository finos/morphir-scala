package org.finos.morphir
package runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, QName, Name, Type, Module}
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

object TestLibrary {
  val fullName = s"./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json"
  val text = Source
    .fromFile(fullName)
    .getLines()
    .mkString("\n")
  val morphirIRFile = text.fromJson[MorphirIRFile]
  val library = morphirIRFile
    .getOrElse(throw new Exception(morphirIRFile.toString))
    .distribution
    .asInstanceOf[Library]
}

object EvaluatorJsonHelpers {

  object Native {
    val plus: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
        ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] + ResultValue.unwrap(b).asInstanceOf[Long])
    )
    val subtract: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
        ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] - ResultValue.unwrap(b).asInstanceOf[Long])
    )
    val negate: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      1,
      (a: ResultValue[Unit, Type.UType]) => ResultValue.Primitive(-ResultValue.unwrap(a).asInstanceOf[Long])
    )
    val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) => {
        val denominator = Math.log(ResultValue.unwrap(a).asInstanceOf[Double])
        val asDouble =
          if (denominator == 0)
            Double.PositiveInfinity
          else
            Math.log(ResultValue.unwrap(b).asInstanceOf[Double]) / denominator
        ResultValue.Primitive(asDouble)
      }
    )

    val lessThan: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
        ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] < ResultValue.unwrap(b).asInstanceOf[Long])
    )
    val cons: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) => {
        val listB = b.asInstanceOf[ResultValue.ListResult[Unit, Type.UType]]
        ResultValue.ListResult(a :: listB.elements)
      }
    )

//    val fileName = "./morphir/toolkit/codec/zio/json/test/exampleIR/myMap.json"
//    val lines = Source.fromFile(fileName).getLines
//    val text = lines.mkString("\n")
//    val morphirIRFile = text.fromJson[MorphirIRFile]
//    val distribution = morphirIRFile.getOrElse(throw new Exception("Ugh")).distribution
//    val library = distribution.asInstanceOf[Library]
    val library    = TestLibrary.library
    val module     = library.packageDef.modules(Module.ModuleName.fromString("MyMap")).value
    val definition = module.values.values.head.value.value
    val map: SDKValue[Unit, Type.UType] = SDKValue.SDKValueDefinition(
      definition
    )

    val pi: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(ResultValue.Primitive(3L))

    val native: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
      FQName.fromString("Morphir.SDK:Basics:pi")              -> pi,
      FQName.fromString("Morphir.SDK:Basics:add")             -> plus,
      FQName.fromString("Morphir.SDK:Basics:subtract")        -> subtract,
      FQName.fromString(" Morphir.SDK:Basics:negate")         -> negate,
      FQName.fromString("Morphir.SDK:Basics:logBase")         -> log,
      FQName.fromString("Morphir.SDK:Basics:lessThan")        -> lessThan,
      FQName.fromString("Morphir.SDK:List:cons")              -> cons,
      FQName.fromString("Morphir.SDK:List:map")               -> map,
      FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
    )
  }

  def eval(moduleName: String, testName: String, lib: Library): Any = {
    val store     = flatten(lib)
    val defName   = FQName.fromString(s"Morphir.Examples.App:$moduleName:$testName")
    val applyNode = V.apply(V.reference(defName), V.unit) :> T.unit // lies but I don't think we check?
    EvaluatorQuick.evaluate[Unit, Type.UType](applyNode, store)
  }

  def flatten(lib: Library): Store[Unit, Type.UType] = {
    val packageName = lib.packageName
    val fqNameBindings = lib.packageDef.modules.flatMap { case (moduleName, accessControlledModule) =>
      val valueDefs: Map[FQName, SDKValue[Unit, Type.UType]] = accessControlledModule.value.values.map {
        case (localName, accessControlledValue) =>
          val name       = FQName(packageName.toPath, moduleName.toPath, localName)
          val definition = accessControlledValue.value.value
          val sdkDef     = SDKValue.SDKValueDefinition(definition)
          (name, sdkDef)
      }
      val typeDefs: Map[FQName, SDKValue[Unit, Type.UType]] = accessControlledModule.value.types.flatMap {
        case (localName, accessControlledType) =>
          val definition = accessControlledType.value.value
          definition match {
            case Type.Definition.CustomType(Chunk(), accessControlledCtors) =>
              val ctors = accessControlledCtors.value.toMap
              ctors.map { case (ctorName, ctorArgs) =>
                val name = FQName(packageName.toPath, moduleName.toPath, ctorName)
                (name, SDKValue.SDKConstructor[Unit, Type.UType](ctorArgs.map(_._2).toList))
              }
            case Type.Definition.CustomType(_, _) =>
              throw new Exception("Unimplemented - type parameters on custom type")
            case Type.Definition.TypeAlias(_, _) => Map.empty
          }
      }
      valueDefs ++ typeDefs
    }
    Store(fqNameBindings ++ Native.native, CallStackFrame(Map(), None))
  }

  def runTest[T](fileName: String, moduleName: String, testName: String): T = {

    val result = eval(moduleName, testName, TestLibrary.library).asInstanceOf[T]
    result
  }
}

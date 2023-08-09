//package org.finos.morphir.runtime.quick
//
//import org.finos.morphir.testing.MorphirBaseSpec
//import org.finos.morphir.ir.{FQName, Module, MorphirIRFile, Name, QName}
//import org.finos.morphir.ir.Type as T
//import org.finos.morphir.ir.Value as V
//import org.finos.morphir.ir.Documented
//
//import scala.collection.immutable.ListMap
//import zio.{test as _, *}
//import zio.prelude.fx.*
//import zio.test.{Result as TestResult, *}
//import zio.test.Assertion.{equalTo, fails}
//import zio.test.TestAspect.{ignore, tag}
//import org.finos.morphir.ir.sdk.Basics
//import ExtensionInterface.*
//import org.finos.morphir.runtime.quick.EvaluatorQuick.IntType
//
//
//object ExtensionInterfaceSpec extends MorphirBaseSpec{
//
//
//
//  def spec = suite("Basic Exploration")(
//        test("Value lookup") {
//          val something = Basics.moduleSpec.values(Name("floor"))
//          val extracted : Any = extract(something)
//          assertTrue(true)
//        },
//        test("Lookup by FQName"){
//          val lookedUp = lookup(FQName.fromString("Morphir.SDK:Basics:round"))
//          val extracted : Any = extract(lookedUp)
//          assertTrue(true)
//        },
//        test("Auto conversion 1"){
//          val nativeFunction : Any= makeNative(FQName.fromString("Morphir.SDK:Basics:round"), (x : Double) => x.toInt)
//          assertTrue (true)
//        },
//        test("Auto conversion 2") {
//          val addName = FQName.fromString("Morphir.SDK:Basics:add")
//          val gennedAdd : Any= ExtensionInterface.makeNative[Unit, T.UType](addName, (arg1: IntType, arg2: IntType) => arg1 + arg2)
//          assertTrue(gennedAdd == "Woo!")
//        }
//  )
//}

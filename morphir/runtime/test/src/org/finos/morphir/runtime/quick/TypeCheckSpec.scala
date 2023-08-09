package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.ir.{Module, MorphirIRFile}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.Value as V
import org.finos.morphir.ir.Documented
import org.finos.morphir.runtime.*
import scala.collection.immutable.ListMap
import zio.{test as _, *}
import zio.prelude.fx.*
import zio.test.{Result as TestResult, *}
import zio.test.Assertion.{equalTo, fails}
import zio.test.TestAspect.{ignore, tag}
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.printing.DetailLevel
import org.finos.morphir.ir.printing.FieldNames
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Type.UType

object TypeCheckSpec extends MorphirBaseSpec {
  val lib = EvaluationLibrary.loadDistribution("./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json")
    .asInstanceOf[Library]

  class ListThing(name: String) {
    val ref          = FQName.fromString(name)
    val (pkg, mod, loc) = (ref.getPackagePath, ref.getModulePath, ref.localName)
    val value           = lib.lookupValueSpecification(PackageName(pkg), ModuleName(mod), loc).get
    val listArg         = value.inputs(0)._2
  }
  val withParam   = new ListThing("Morphir.Examples.App:TypeCheckerTests:withParam")
  val withInt     = new ListThing("Morphir.Examples.App:TypeCheckerTests:withInt")
  val listConcept = Concept.List(Concept.Integer)
  val toType      = ToMorphirType.summon[Concept]
  val intListType = toType.withAttributesOf(listConcept).morphirType

  def spec = suite("Exploration")(
//    suite("Lookup")(
//      test("withParam") {
//        assertTrue(PrintIR(withParam.value).toString() == "Yes")
//      },
//      test("withInt") {
//        assertTrue(PrintIR(withInt.value).toString() == "Yes")
//      }
//    ),
//    suite("listArg")(
//      test("withParam") {
//        assertTrue(PrintIR(withParam.listArg, DetailLevel.Medium).toString() == "listArg")
//      },
//      test("withInt") {
//        assertTrue(PrintIR(withInt.listArg, DetailLevel.Medium).toString() == "listArg")
//      }
//    ),
    suite("Int List Type Checks")(
      test("Concept") {
        val mapped = Utils.typeCheckArg(intListType, withInt.listArg, Map())
        assertTrue(mapped == Right(Map[Name, UType]()))
      }
    ),
    suite("Param List Type Checks")(
      test("Concept") {
        val mapped = Utils.typeCheckArg(intListType, withParam.listArg, Map())
        assertTrue(mapped == Right(Map(Name("a") -> Basics.intType)))
      }
    )
  )

}

package org.finos.morphir.runtime.quick

import org.finos.morphir.datamodel.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.ir.printing.{DetailLevel, FieldNames, PrintIR}
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.{Documented, Module, MorphirIRFile, Type => T, Value => V}
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.*
import org.finos.morphir.testing.MorphirBaseSpec
import zio.prelude.fx.*
import zio.test.Assertion.{equalTo, fails}
import zio.test.TestAspect.{ignore, tag}
import zio.test.{Result as TestResult, *}
import zio.{test as _, *}

import scala.collection.immutable.ListMap

object GatherRefsSpec extends MorphirBaseSpec {
  val dist = EvaluationLibrary.loadDistributionUnsafe("./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json")
  // Pretend the following is a distinct distribution like an SDK (when using the instrument, it should be one or more such)
  val otherDist =
    EvaluationLibrary.loadDistributionUnsafe("./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json")
  val example: FQName = FQName.fromString("Morphir.SDK:Basics:and")
  val existing        = Native.native.keys

  def spec = suite("Exploration")(
    suite("From Distribution")(
      test("Everything from Tests") {
        val stuff = GatherReferences.fromDistributions(dist)
        val defs  = stuff.definitions.map(_.toString).mkString("\n")
        assertTrue(defs == "")
      },
      test("SDK Defs from Tests") {
        val stuff  = GatherReferences.fromDistributions(dist)
        val defs   = stuff.definitions.filter(_.getPackagePath == example.getPackagePath)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")
      },
      test("SDK Defs from Distribution") {
        val stuff  = GatherReferences.fromDistributions(dist)
        val defs   = stuff.definitions.filter(_.getPackagePath == example.getPackagePath)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")
      },
      test("SDK Defs from Everywhere") {
        val stuff  = GatherReferences.fromDistributions(dist, otherDist)
        val defs   = stuff.definitions.filter(_.getPackagePath == example.getPackagePath)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")
      },
      test("Missing Defs from Tests") {
        val stuff  = GatherReferences.fromDistributions(dist)
        val defs   = stuff.definitions.filter(_.getPackagePath == example.getPackagePath).diff(existing.toSet)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")
      },
      test("Missing SDK Defs from Everywhere") {
        val stuff  = GatherReferences.fromDistributions(dist, otherDist)
        val defs   = stuff.definitions.filter(_.getPackagePath == example.getPackagePath).diff(existing.toSet)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")
      },
      test("Only actually used") {
        val stuff     = GatherReferences.fromDistributions(dist)
        val moreStuff = GatherReferences.fromEntrySet(stuff, dist, otherDist)
        val defs   = moreStuff.definitions // .filter(_.getPackagePath == example.getPackagePath)//.diff(existing.toSet)
        val string = defs.map(_.toString).mkString("\n")
        assertTrue(string == "")

      }
    )
  ) @@ ignore @@ TestAspect.tag("Exploration code, not ready to be actual test")

}

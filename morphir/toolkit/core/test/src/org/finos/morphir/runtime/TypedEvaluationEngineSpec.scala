package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import ir.Name
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.Value as V

import scala.collection.immutable.ListMap
import zio.{test as _, *}
import zio.prelude.fx.*
import zio.test.*
import zio.test.TestAspect.{ignore, tag}
import EvaluationEngine.*
import V.*
import org.finos.morphir.runtime
import org.finos.morphir.runtime.{EvaluationError, MorphirRecord}

trait TypedEvaluationEngineSpec { self: MorphirBaseSpec =>
  def typedEvaluationEngineSuite =
    suite("For TypedValue")(
      letDefinitionSuite,
      listSuite,
      literalSuite,
      unitSuite,
      ifThenElseSuite,
      recordSuite,
      variableSuite,
      tupleSuite
    ).provide(
      ZLayer.succeed(EvaluationEngine.typed)
    )

  def tupleSuite = suite("Tuple")(
    suite("Of Literals")(
      test("Should be possible to evaluate a Tuple of literal values") {
        val value: TypedValue = V.tuple(
          V.boolean(true)    -> ir.sdk.Basics.boolType,
          V.string("Batman") -> ir.sdk.String.stringType,
          V.float(42.5)      -> ir.sdk.Basics.floatType
        )
        val context = Context.Typed.createRoot()
        for {
          actual <- evaluateZIO(value, context)
        } yield assertTrue(actual == (true, "Batman", 42.5))
      },
      suite("Of Variables")(
        test("Should resolve nested variables") {
          val aName = Name.fromString("a")
          val nName = Name.fromString("n")
          val value = V.tuple(V.variable(aName) -> ir.sdk.Char.charType, V.variable(nName) -> ir.sdk.Basics.intType)
          val context = EvaluationEngine.Context.Typed.createRoot(
            Var(aName) := 'A',
            Var(nName) := 42
          )
          for {
            actual <- evaluateZIO(value, context)
          } yield assertTrue(actual == ('A', 42))
        }
      ),
      TupleAritySuite.evaluatesTupleArities2to22Suite
    )
  )

  def ifThenElseSuite = suite("ifThenElse")(
    test("Should evaluate true condition to ThenBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(true), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 2)
    },
    test("Should evaluate false condition to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(false), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 6)
    },
    test("Should evaluate any other non true conditions to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.int(3), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 6)
    }
  )

  def letDefinitionSuite = suite("LetDefinition")(
    test("Should support simple let involving an Int literal") {
      val value   = V.let("n", 42, V.variable("n") :> ir.sdk.Basics.intType)
      val context = Context.Typed.createRoot()
      for {
        // initalVars <-  ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
        actual <- evaluateZIO(value, context)
        // finalVars  <- ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
      } yield assertTrue(actual == 42 /*, initalVars == finalVars*/ )
    },
    test("Should support nested let definitions") {

      // Let x = 3 in ((let x = 2 in x), x)
      val innerLet: TypedValue = V.let("x", 2, V.variable("x") :> ir.sdk.Basics.intType)
      val value =
        V.let(
          "x",
          3,
          V.tuple(
            T.tuple(ir.sdk.Basics.intType, ir.sdk.Basics.intType),
            innerLet,
            V.variable("x") :> ir.sdk.Basics.intType
          )
        )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == (2, 3))
    }
  )

  def listSuite = suite("List")(
    suite("Of Literals")(
      test("Should evaluate a List of Booleans") {
        val value: TypedValue = V.listOf(
          ir.sdk.Basics.boolType,
          V.boolean(true) :> ir.sdk.Basics.boolType,
          V.boolean(false) :> ir.sdk.Basics.boolType
        )
        val context = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- evaluateZIO(value, context)
        } yield assertTrue(actual == List(true, false))
      },
      test("Should evaluate a List of Ints") {
        val value: TypedValue = V.listOf(
          ir.sdk.Basics.intType,
          V.int(10) :> ir.sdk.Basics.intType,
          V.int(20) :> ir.sdk.Basics.intType,
          V.int(30) :> ir.sdk.Basics.intType,
          V.int(40) :> ir.sdk.Basics.intType
        )
        val context = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- EvaluationEngine.evaluateZIO(value, context)
        } yield assertTrue(actual == List(10, 20, 30, 40))
      }
    )
  )

  def literalSuite = suite("Literal")(
    test("Should evaluate True to true") {
      val value: TypedValue = V.boolean(true) :> ir.sdk.Basics.boolType
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == true)
    },
    test("Should evaluate False to false") {
      val value: TypedValue = V.boolean(false) :> ir.sdk.Basics.boolType
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == false)
    },
    test("Should evaluate a String Literal to its value") {
      check(Gen.string) { strValue =>
        val value: TypedValue = V.string(strValue) :> ir.sdk.String.stringType
        val context           = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- EvaluationEngine.evaluateZIO(value, context)
        } yield assertTrue(actual == strValue)
      }
    }
  )

  def recordSuite = suite("Record")(
    test("Should work with Literals") {
      val firstNameField = "firstName" -> (V.string("John") :> ir.sdk.String.stringType)
      val lastNameField  = "lastName"  -> (V.string("Doe") :> ir.sdk.String.stringType)
      val ageField       = "age"       -> (V.int(26) :> ir.sdk.Basics.intType)
      val recordFields = Chunk(
        "lastName"  -> ir.sdk.String.stringType,
        "firstName" -> ir.sdk.String.stringType,
        "age"       -> ir.sdk.Basics.intType
      ).map(T.field(_))

      val recordType = T.record(recordFields)

      val value: TypedValue = V.record(recordType, lastNameField, firstNameField, ageField)
      val context           = EvaluationEngine.Context.Typed.createRoot()
      val expected = MorphirRecord(
        ListMap(
          Name.fromString("lastName")  -> "Doe",
          Name.fromString("firstName") -> "John",
          Name.fromString("age")       -> 26
        )
      )

      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
        actualRecord = actual.asInstanceOf[MorphirRecord]
      } yield assertTrue(
        actual == expected,
        actualRecord.firstName == "John",
        actualRecord.lastName == "Doe",
        actualRecord.age == 26,
        actualRecord.productElementName(0) == expected.productElementName(0),
        actualRecord.productElementName(1) == expected.productElementName(1),
        actualRecord.productElementName(2) == expected.productElementName(2)
      )
    },
    test("Should work with nested records") {
      val heightField = "height" -> (V.float(12.3) :> ir.sdk.Basics.floatType)
      val lengthField = "length" -> (V.float(11.2) :> ir.sdk.Basics.floatType)
      val uomField    = "uom"    -> (V.string("cm") :> ir.sdk.String.stringType)
      val nestedRecordFields = Chunk(
        "height" -> ir.sdk.Basics.floatType,
        "length" -> ir.sdk.Basics.floatType,
        "uom"    -> ir.sdk.String.stringType
      ).map(T.field(_))

      val nestedRecordType         = T.record(nestedRecordFields)
      val nestedRecord: TypedValue = V.record(nestedRecordType, heightField, lengthField, uomField)

      val itemField  = "item"  -> (V.string("paper") :> ir.sdk.String.stringType)
      val sizesField = "sizes" -> nestedRecord
      val stockField = "stock" -> (V.boolean(true) :> ir.sdk.Basics.boolType)
      val recordFields = Chunk(
        "item"  -> ir.sdk.Basics.floatType,
        "sizes" -> nestedRecordType,
        "stock" -> ir.sdk.Basics.boolType
      ).map(T.field(_))

      val recordType = T.record(recordFields)

      val value: TypedValue = V.record(recordType, itemField, sizesField, stockField)
      val context           = EvaluationEngine.Context.Typed.createRoot()

      val sizesRecord = MorphirRecord(
        ListMap(Name.fromString("height") -> 12.3, Name.fromString("length") -> 11.2, Name.fromString("uom") -> "cm")
      )
      val expected = MorphirRecord(
        ListMap(
          Name.fromString("item")  -> "paper",
          Name.fromString("sizes") -> sizesRecord,
          Name.fromString("stock") -> true
        )
      )

      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
        actualRecord       = actual.asInstanceOf[MorphirRecord]
        actualNestedRecord = actualRecord.sizes.asInstanceOf[MorphirRecord]
      } yield assertTrue(
        actual == expected,
        actualRecord.item == "paper",
        actualRecord.sizes == sizesRecord,
        actualRecord.stock == true,
        actualNestedRecord.height == 12.3,
        actualNestedRecord.length == 11.2,
        actualNestedRecord.uom == "cm"
      )
    }
  )

  def unitSuite: Spec[EvaluationEngine[Unit, T.Type[Unit]], EvaluationError] = suite("Unit")(
    test("Should be possible to evaluate a Unit value") {
      val value: TypedValue = V.unit(T.unit)
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == ())
    }
  )

  def variableSuite = suite("Variable")(
    test("Should be possible to resolve a variable that has the Scala unitValue") {
      val varName = Name.fromString("testVar")
      val value   = V.variable(varName) :> T.unit
      val context = EvaluationEngine.Context.Typed.createRoot(
        Var(varName) := ()
      )
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
        _      <- Console.printLine(s"Actual has a type of ${actual.getClass.getSimpleName}")
      } yield assertTrue(actual == ())
    },
    test("Should be possible to resolve a variable that has a Scala boolean value") {
      val trueVarName  = Name.fromString("trueVar")
      val falseVarName = Name.fromString("falseVar")
      val trueValue    = V.variable(trueVarName) :> ir.sdk.Basics.boolType
      val falseValue   = V.variable(falseVarName) :> ir.sdk.Basics.boolType

      val context = EvaluationEngine.Context.Typed.createRoot(
        Var(trueVarName)  := true,
        Var(falseVarName) := false
      )
      for {
        actualTrueValue  <- EvaluationEngine.evaluateZIO(trueValue, context)
        actualFalseValue <- EvaluationEngine.evaluateZIO(falseValue, context)
      } yield assertTrue(actualTrueValue == true, actualFalseValue == false)
    }
  )
}

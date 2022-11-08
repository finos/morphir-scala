package org.finos.morphir.interpreter

import java.math.BigInteger
import zio.test._
import zio.test.TestAspect.{ignore, tag}
import org.finos.morphir.ir.Name
import org.finos.morphir.ir.Value.RawValue
import org.finos.morphir.IR
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.Value._
import org.finos.morphir.ir.NativeFunction
import org.finos.morphir.ir.FQName
import org.finos.morphir.ir.NativeFunction._
import org.finos.morphir.ir.Value._
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.IR.TypeConstructorInfo
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.testing.CaseExample._

object InterpreterSpec extends MorphirBaseSpec {
  val sampleIR = IR(
    valueSpecifications = Map.empty,
    valueDefinitions = Map.empty,
    typeSpecifications = Map(recordTypeName -> recordTypeAliasSpecification),
    typeConstructors = Map(
      savingsAccountTypeName  -> savingsAccountTypeConstructor,
      checkingAccountTypeName -> checkingAccountTypeConstructor
    )
  )
  def evaluate(value: RawValue): Any = Interpreter.evaluate(value, sampleIR, Map.empty)

  def spec = suite("Interpreter")(
    // suite("native functions")(
    //   suite("addition")(
    //     test("Should evaluate correctly") {
    //       assertTrue(
    //         evaluate(additionExample) == Right(new BigInteger("3"))
    //       )
    //     }
    //   ),
    //   suite("subtraction")(
    //     test("Should evaluate correctly") {
    //       assertTrue(evaluate(subtractionExample) == Right(new BigInteger("-1")))
    //     }
    //   )
    // ),
    suite("tuple case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(tupleCaseExample) == Right((new BigInteger("1"), new BigInteger("2"))))
      }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    suite("list case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(listCaseExample) == Right(scala.List("hello", "world")))
      }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    suite("if then else case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(ifThenElseCaseExample) == Right("no"))
      }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    suite("record case")(
      test("Should evaluate correctly") {
        assertTrue(
          evaluate(recordCaseExample) == Right(
            Map(
              Name.unsafeMake(scala.List("field", "a")) -> "hello",
              Name.unsafeMake(scala.List("field", "b")) -> new BigInteger("2")
            )
          )
        )
      },
      test("Should update correctly") {
        assertTrue(
          evaluate(recordCaseUpdateExample) == Right(
            Map(
              Name.unsafeMake(scala.List("field", "a")) -> "hello",
              Name.unsafeMake(scala.List("field", "b")) -> new BigInteger("3")
            )
          )
        )
      }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    // suite("let recursion case")(
    // test("Multiple bindings that do not refer to each other") {
    //   assertTrue(evaluate(letIntroduceMultipleExample) == Right(new BigInteger("42")))
    // },
    // test("Multiple bindings where earlier binding refers to later definition") {
    //   assertTrue(evaluate(letIntroduceOutOfOrderExample) == Right(new BigInteger("44")))
    // }
    // test("recursive let definition example") {
    //   assertTrue(evaluate(letRecExample) == Right(new BigInteger("6")))
    // },
    // test("Static scoping example") {
    //   assertTrue(evaluate(staticScopingExample) == Right("static"))
    // }
    // ),
    suite("let non recursion case")(
      test("Let destructor case") {
        assertTrue(evaluate(letDestructExample) == Right("red"))
      }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    suite("apply case")(
      test("Apply field function") {
        assertTrue(evaluate(applyFieldFunction) == Right("hello"))
      },
      test("Apply lambda with wildcard") {
        assertTrue(evaluate(applyWithWildCard) == Right(new BigInteger("42")))
      }
      // test("Lambda defined in let") {
      //   assertTrue(evaluate(lambdaExample) == Right(new BigInteger("66")))
      // }
    ) @@ ignore @@ tag("Interpreter Not Ready"),
    // suite("constructor case")(
    //   test("Should evaluate correctly XYZ") {
    //     assertTrue(
    //       evaluate(constructorExample) == Right(
    //         GenericCaseClass.fromFields(recordTypeName, Name("name") -> "Adam", Name("age") -> new BigInteger("42"))
    //       )
    //     )
    //   },
    //   test("Custom type should evaluate correctly") {
    //     assertTrue(
    //       evaluate(savingsAccountConstructorExample) == Right(
    //         GenericCaseClass.fromFields(
    //           savingsAccountTypeName,
    //           Name("arg1") -> "Adam"
    //         )
    //       ),
    //       evaluate(checkingAccountConstructorExample) == Right(
    //         GenericCaseClass.fromFields(
    //           checkingAccountTypeName,
    //           Name("arg1") -> "Brad",
    //           Name("arg2") -> new BigInteger("10000")
    //         )
    //       )
    //     )
    //   }
    // ),
    suite("pattern matching")(
      suite("literal")(),
      suite("wildcard")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchWildcardCaseExample) == Right(new BigInteger("100")))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      suite("as")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchAsCaseExample) == Right(new BigInteger("42")))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      // suite("as with literal")(
      //   test("Should evaluate correctly") {
      //     assertTrue(evaluate(patternMatchAsCaseComplexExample) == Right(new BigInteger("14")))
      //   }
      // ),
      suite("tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleCaseExample) == Right(new BigInteger("107")))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      suite("singleton tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleOneCaseExample) == Right("singleton tuple"))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      suite("singleton non match tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleOneCaseCounterExample) == Right("right"))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      // suite("constructor")(
      //   test("Should evaluate correctly") {
      //     assertTrue(evaluate(patternConstructorCaseExample) == Right(new BigInteger("10000")))
      //   }
      // ),
      // suite("head tail list")(
      //   test("Should evaluate correctly") {
      //     assertTrue(evaluate(patternHeadTailCaseExample) == Right(List("world")))
      //   }
      // ),
      suite("empty list")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchEmptyListCaseExample) == Right("empty list"))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready"),
      suite("unit")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternUnitCaseExample) == Right("right"))
        }
      ) @@ ignore @@ tag("Interpreter Not Ready")
    )
  )
}

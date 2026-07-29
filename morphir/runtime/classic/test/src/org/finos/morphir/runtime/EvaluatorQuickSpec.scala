//package org.finos.morphir
//package runtime
//
//import org.finos.morphir.testing.MorphirBaseSpec
//import ir.Name
//import org.finos.morphir.ir.{Type => T}
//import org.finos.morphir.ir.{Value => V}
//
//import scala.collection.immutable.ListMap
//import zio.{test as _, *}
//import zio.prelude.fx.*
//import zio.test.*
//import zio.test.Assertion.{equalTo, fails}
//import zio.test.TestAspect.{ignore, tag}
//import org.finos.morphir.ir.{FQName, Name, Type}
//import org.finos.morphir.runtime.quick.EvaluatorQuick.*
//import V.*
//import org.finos.morphir.ir.Type
//import org.finos.morphir.ir.Literal.Literal as Lit
//import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}
//
//object EvaluatorQuickSpec extends MorphirBaseSpec {
//  def eval(v: TypedValue) =
//    EvaluatorQuick.evaluate[Unit, Type.UType](v, Store.empty[Unit, Type.UType])
//
//  def spec = suite("Basic Evaluation")(
//    suite("Literal Spec")(
//      test("Boolean") {
//        val value: TypedValue = V.boolean(true) :> ir.sdk.Basics.boolType
//        val result            = eval(value)
//        assertTrue(result == true)
//      }
//    ),
//    suite("LetDefinition Spec")(
//      test("Unused simple binding") {
//        val value: TypedValue = V.let("n", 42, V.boolean(true) :> ir.sdk.Basics.boolType)
//        val result            = eval(value)
//        assertTrue(result == true)
//      },
//      test("Simple binding") {
//        val value: TypedValue = V.let("n", 42, V.variable("n") :> ir.sdk.Basics.intType)
//        val result            = eval(value)
//        assertTrue(result == 42)
//      }
//      // TODO: Show that variables leave scope later in sequential ops
//      // TODO: Test let definition
//    ),
//    suite("NativeFunction Spec")(
//      test("Addition") {
//        val reference = V.reference(
//          FQName.fromString("Morphir.SDK:Basics:add")
//        ) // :> T.function(ir.sdk.Basics.intType, T.function(ir.sdk.Basics.intType, ir.sdk.Basics.intType))
//        val curried = V.apply(
//          reference,
//          V.int(5) // :> ir.sdk.Basics.intType
//        )          // :> T.function(ir.sdk.Basics.intType, ir.sdk.Basics.intType)
//        val value = V.apply(
//          curried,
//          V.int(7)
//        ) :> ir.sdk.Basics.intType
//        val result = eval(value)
//        assertTrue(result == 12)
//      }
//    ),
//    suite("Destructure")(
//      test("Should work with tuples") {
//        // Let (a,b) = c in b
//        val attributes = T.tupleVar(ir.sdk.Basics.intType, ir.sdk.String.stringType)
//        val pat =
//          V.tuplePattern(attributes, asAlias(ir.sdk.Basics.intType, "a"), asAlias(ir.sdk.String.stringType, "b"))
//        val c  = V.tuple(V.int(26) -> ir.sdk.Basics.intType, V.string("Success") -> ir.sdk.String.stringType)
//        val in = V.variable(ir.sdk.String.stringType, "b")
//        val value: TypedValue = V.destructure(attributes, pat, c, in)
//        val result            = eval(value)
//        assertTrue(result == "Success")
//      },
//      test("Should be able to switch tuple variables") {
//        // Let (a,b) = c in (b,a)
//        val attributes = T.tupleVar(ir.sdk.Basics.intType, ir.sdk.Basics.floatType, ir.sdk.String.stringType)
//        val pat = V.tuplePattern(
//          attributes,
//          asAlias(ir.sdk.Basics.intType, "a"),
//          asAlias(ir.sdk.Basics.floatType, "b"),
//          asAlias(ir.sdk.String.stringType, "c")
//        )
//        val c = V.tuple(
//          V.int(26)       -> ir.sdk.Basics.intType,
//          V.float(24.678) -> ir.sdk.Basics.floatType,
//          V.string("Joe") -> ir.sdk.String.stringType
//        )
//        val inAttributes = T.tupleVar(ir.sdk.String.stringType, ir.sdk.Basics.floatType, ir.sdk.Basics.intType)
//        val in = V.tuple(
//          inAttributes,
//          V.variable(ir.sdk.String.stringType, "c"),
//          V.variable(ir.sdk.Basics.floatType, "b"),
//          V.variable(ir.sdk.Basics.intType, "a")
//        )
//        val value: TypedValue = V.destructure(attributes, pat, c, in)
//        val result            = eval(value)
//        assertTrue(result == ("Joe", 24.678, 26))
//      },
//      test("Should work with lists") {
//        val pat = V.headTailPattern(V.asAlias("first"), V.headTailPattern(V.asAlias("second"), V.wildcardPattern))
//        val c   = V.list(V.int(10), V.int(20), V.int(30), V.int(40))
//        val in  = V.variable("second")
//        val value: TypedValue = V.destructure(pat, c, in) :> ir.sdk.Basics.intType
//        val result            = eval(value)
//        assertTrue(result == 20)
//      },
//      test("Should be able to create a tuple from the first 2 elements in a list in reverse order") {
//        val pat               = V.headTailPattern(V.asAlias("a"), V.headTailPattern(V.asAlias("b"), V.wildcardPattern))
//        val c                 = V.list(V.int(10), V.float(20.11), V.int(30), V.int(40))
//        val in                = V.tuple(V.variable("b"), V.variable("a"))
//        val value: TypedValue = V.destructure(pat, c, in) :> ir.sdk.Basics.intType
//        val result            = eval(value)
//        assertTrue(result == (20.11, 10))
//      },
//      test("Should work with LiteralPattern") {
//        val pat               = V.asPattern(V.literalPattern(Lit.float(125.345)), "a")
//        val c                 = V.float(125.345)
//        val in                = V.variable("a")
//        val value: TypedValue = V.destructure(pat, c, in) :> ir.sdk.Basics.floatType
//        val result            = eval(value)
//        assertTrue(result == 125.345)
//      }
//    ),
//    suite("LetDefinition")(
//      test("Should support simple let involving an Int literal") {
//        val value  = V.let("n", 42, V.variable("n") :> ir.sdk.Basics.intType)
//        val result = eval(value)
//        assertTrue(result == 42)
//      },
//      test("Should support shadowing outer let definitions") {
//        val innerLet: TypedValue = V.let("x", 2, V.variable("x") :> ir.sdk.Basics.intType)
//        val value =
//          V.let(
//            "x",
//            3,
//            V.tuple(
//              T.tupleVar(ir.sdk.Basics.intType, ir.sdk.Basics.intType),
//              innerLet,
//              V.variable("x") :> ir.sdk.Basics.intType
//            )
//          )
//        val result = eval(value)
//        assertTrue(result == (2, 3))
//      }
//    ),
//    suite("PatternMatch")(
//      test("Should support naming a variable") {
//        val patternCase = V.asAlias("patternVariable")
//        val value: TypedValue =
//          V.patternMatch(
//            V.int(55),
//            Chunk((patternCase, V.variable("patternVariable")))
//          ) :> ir.sdk.Basics.intType
//        val result = eval(value)
//        assertTrue(result == 55)
//      },
//      test("Should support nested as patterns wrapping a literal") {
//        val patternCase = V.asPattern(V.asPattern(V.literalPattern(Lit.int(125)), "red"), "blue")
//        val badTuple =
//          V.tuple(
//            V.int(1),
//            V.int(2)
//          )
//        val value: TypedValue =
//          V.patternMatch(
//            V.int(125),
//            Chunk(
//              (
//                V.literalPattern(Lit.int(123)),
//                V.string("This case should not have matched")
//              ),
//              (
//                patternCase,
//                V.tuple(
//                  V.variable("red"),
//                  V.variable("blue")
//                )
//              ),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> T.tupleVar(ir.sdk.Basics.intType, ir.sdk.Basics.intType)
//        val result = eval(value)
//        assertTrue(result == (125, 125))
//      },
//      test("Should always match against a wildcard") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.int(234),
//            Chunk(
//              (
//                V.literalPattern(Lit.int(123)),
//                V.string("This case should not have matched")
//              ),
//              (V.wildcardPattern, V.string("Wildcard Matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Wildcard Matched")
//      },
//      test("Should match against a literal iff it is the value") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.int(234),
//            Chunk(
//              (
//                V.literalPattern(Lit.int(123)),
//                V.string("This case should not have matched")
//              ),
//              (V.literalPattern(Lit.int(234)), V.string("Correct!")),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Correct!")
//      },
//      test("Should match against a unit pattern only with a unit") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.unit,
//            Chunk(
//              (
//                V.literalPattern(Lit.int(123)),
//                V.string("This case should not have matched")
//              ),
//              (V.unitPattern, V.string("Correct!")),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Correct!")
//      },
////    test("Should match against an empty list pattern only with an empty list") {
////      val value: TypedValue =
////        V.patternMatch(
////          V.emptyList,
////          Chunk(
////            (
////              V.literalPattern(Lit.int(123)),
////              V.string("This case should not have matched")
////            ),
////            (V.emptyListPattern, V.string("Correct!")),
////            (V.wildcardPattern, V.string("An earlier case should have been matched"))
////          )
////        ) :> ir.sdk.String.stringType
////        val result = eval(value)
////        assertTrue(result == "Correct!")
////    },
//      test("Should extract the head of a list") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.list(
//              V.int(10),
//              V.int(20),
//              V.int(30),
//              V.int(40)
//            ),
//            Chunk(
//              (V.emptyListPattern, V.string("This case should not have matched")),
//              (V.headTailPattern(V.asAlias("head"), V.wildcardPattern), V.variable("head")),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == 10)
//      },
//      test("Should combine values from head and tail patterns") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.list(
//              V.int(10),
//              V.int(20),
//              V.int(30),
//              V.int(40)
//            ),
//            Chunk(
//              (V.emptyListPattern, V.string("This case should not have matched")),
//              (V.headTailPattern(V.asAlias("head"), V.emptyListPattern), V.string("This case should not have matched")),
//              (
//                V.headTailPattern(V.asAlias("head"), V.headTailPattern(V.asAlias("second"), V.wildcardPattern)),
//                V.tuple(
//                  V.variable("second"),
//                  V.variable("head")
//                )
//              ),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == (20, 10))
//      },
//      test("Should recognize a list of one elenent") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.list(
//              V.int(10)
//            ),
//            Chunk(
//              (V.emptyListPattern, V.string("This case should not have matched")),
//              (V.headTailPattern(V.wildcardPattern, V.emptyListPattern), V.string("Correct!")),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Correct!")
//      },
//      test("Should match a tuple") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.tuple(
//              V.int(11),
//              V.int(12)
//            ),
//            Chunk(
//              (V.emptyListPattern, V.string("This case should not have matched")),
//              (
//                V.tuplePattern(V.asAlias("first"), V.asAlias("second"), V.wildcardPattern),
//                V.string("This case should not have matched")
//              ),
//              (
//                V.tuplePattern(V.asAlias("first"), V.asAlias("second")),
//                V.tuple(V.variable("second"), V.variable("first"))
//              ),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == (12, 11))
//      },
//      test("Should match a tuple") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.tuple(
//              V.int(1),
//              V.int(2)
//            ),
//            Chunk(
//              (
//                V.tuplePattern(V.literalPattern(Lit.int(1)), V.literalPattern(Lit.int(1))),
//                V.string("This case should not have matched")
//              ),
//              (
//                V.tuplePattern(V.literalPattern(Lit.int(2)), V.literalPattern(Lit.int(2))),
//                V.string("This case should not have matched")
//              ),
//              (
//                V.tuplePattern(V.literalPattern(Lit.int(1)), V.literalPattern(Lit.int(2))),
//                V.string("Correct!")
//              ),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Correct!")
//      },
//      test("Should bind both a tuple and its contents") {
//        val value: TypedValue =
//          V.patternMatch(
//            V.tuple(
//              V.int(11),
//              V.int(12)
//            ),
//            Chunk(
//              (V.emptyListPattern, V.string("This case should not have matched")),
//              (
//                V.tuplePattern(V.asAlias("first"), V.asAlias("second"), V.wildcardPattern),
//                V.string("This case should not have matched")
//              ),
//              (
//                V.asPattern(V.tuplePattern(V.asAlias("first"), V.wildcardPattern), "both"),
//                V.tuple(V.variable("both"), V.variable("first"))
//              ),
//              (V.wildcardPattern, V.string("An earlier case should have been matched"))
//            )
//          ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == ((11, 12), 11))
//      }
//    ),
//    suite("List")(
//      suite("Of Literals")(
//        test("Should evaluate a List of Booleans") {
//          val value: TypedValue = V.listOf(
//            ir.sdk.Basics.boolType,
//            V.boolean(true) :> ir.sdk.Basics.boolType,
//            V.boolean(false) :> ir.sdk.Basics.boolType
//          )
//          val result = eval(value)
//          assertTrue(result == List(true, false))
//        },
//        test("Should evaluate a List of Ints") {
//          val value: TypedValue = V.listOf(
//            ir.sdk.Basics.intType,
//            V.int(10) :> ir.sdk.Basics.intType,
//            V.int(20) :> ir.sdk.Basics.intType,
//            V.int(30) :> ir.sdk.Basics.intType,
//            V.int(40) :> ir.sdk.Basics.intType
//          )
//          val result = eval(value)
//          assertTrue(result == List(10, 20, 30, 40))
//        }
//      )
//    ),
//    suite("Apply Lambdas")(
//      test("Should lazily evaluate a literal") {
//        val lambda = V.lambda(
//          V.unitPattern,
//          V.string("Correct!")
//        )
//        val value = V.apply(
//          lambda,
//          V.unit
//        ) :> ir.sdk.String.stringType
//        val result = eval(value)
//        assertTrue(result == "Correct!")
//      },
//      test("Should make input into a tuple") {
//        val lambda = V.lambda(
//          V.asAlias("x"),
//          V.tuple(V.variable("x"), V.variable("x"))
//        )
//        val value = V.apply(
//          lambda,
//          V.int(12)
//        ) :> T.tupleVar(ir.sdk.Basics.intType, ir.sdk.Basics.intType)
//        val result = eval(value)
//        assertTrue(result == (12, 12))
//      }
////      test("Should use the closure where the lambda was defined") {
////        val lambda = V.letRaw(
////          "x",
////          11,
////          V.lambda(
////            V.asAlias("y"),
////            V.tuple(V.variable("x"), V.variable("y"))
////          )
////        )
////        val value = V.apply(
////          lambda,
////          V.int(12)
////        ) :> T.tuple(ir.sdk.Basics.intType, ir.sdk.Basics.intType)
////        val result = eval(value)
////        assertTrue(result == (11, 12))
////      }
//
//    )
//  )
//}

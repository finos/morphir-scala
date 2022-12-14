package org.finos.morphir
package toolkit

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V}
import zio.test._
import EvaluationEngine.{Context, evaluateZIO}

object TupleAritySuite {

  private def name(i: Int) = s"element$i"

  private def element(i: Int) = V.string(name(i)) -> ir.sdk.String.stringType

  def evaluatesTupleArities2to22Suite = suite("Of arities from 2 up to 23")(
    test("Should be possible to evaluate a tuple of arity 2") {
      val value    = V.tuple(element(1), element(2))
      val expected = (name(1), name(2))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 3") {
      val value    = V.tuple(element(1), element(2), element(3))
      val expected = (name(1), name(2), name(3))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 4") {
      val value    = V.tuple(element(1), element(2), element(3), element(4))
      val expected = (name(1), name(2), name(3), name(4))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 5") {
      val value    = V.tuple(element(1), element(2), element(3), element(4), element(5))
      val expected = (name(1), name(2), name(3), name(4), name(5))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 6") {
      val value    = V.tuple(element(1), element(2), element(3), element(4), element(5), element(6))
      val expected = (name(1), name(2), name(3), name(4), name(5), name(6))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 7") {
      val value    = V.tuple(element(1), element(2), element(3), element(4), element(5), element(6), element(7))
      val expected = (name(1), name(2), name(3), name(4), name(5), name(6), name(7))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 8") {
      val value =
        V.tuple(element(1), element(2), element(3), element(4), element(5), element(6), element(7), element(8))
      val expected = (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 9") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9)
      )
      val expected = (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 10") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10)
      )
      val expected = (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9), name(10))
      val context  = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 11") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11)
      )
      val expected =
        (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9), name(10), name(11))
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 12") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12)
      )
      val expected =
        (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9), name(10), name(11), name(12))
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 13") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 14") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 15") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 16") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 17") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 18") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17),
        name(18)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 19") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18),
        element(19)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17),
        name(18),
        name(19)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 20") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18),
        element(19),
        element(20)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17),
        name(18),
        name(19),
        name(20)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 21") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18),
        element(19),
        element(20),
        element(21)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17),
        name(18),
        name(19),
        name(20),
        name(21)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should be possible to evaluate a tuple of arity 22") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18),
        element(19),
        element(20),
        element(21),
        element(22)
      )
      val expected = (
        name(1),
        name(2),
        name(3),
        name(4),
        name(5),
        name(6),
        name(7),
        name(8),
        name(9),
        name(10),
        name(11),
        name(12),
        name(13),
        name(14),
        name(15),
        name(16),
        name(17),
        name(18),
        name(19),
        name(20),
        name(21),
        name(22)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == expected)
    },
    test("Should not evaluate tuples of arity 23") {
      val value = V.tuple(
        element(1),
        element(2),
        element(3),
        element(4),
        element(5),
        element(6),
        element(7),
        element(8),
        element(9),
        element(10),
        element(11),
        element(12),
        element(13),
        element(14),
        element(15),
        element(16),
        element(17),
        element(18),
        element(19),
        element(20),
        element(21),
        element(22),
        element(23)
      )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context).either
      } yield assertTrue(actual.isLeft)
    }
  )
}

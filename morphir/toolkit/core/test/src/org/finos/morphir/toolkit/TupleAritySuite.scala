package org.finos.morphir
package toolkit

import org.finos.morphir.ir.Value as V
import zio.test.*
import org.finos.morphir.toolkit.EvaluationWithTypedValueVisitorSpecs.eval

object TupleAritySuite {

  def evaluatesTupleArities2to22Suite = suite("Of arities from 2 up to 23")(
    (tupleArityTests2to22 :+ negativeCase23): _*
  )

  def tupleArityTests2to22 = tupleArityCases2to22
    .map[Spec[Any, EvaluationError]] { case (testName, tupleTv, expected) =>
      test(testName) {
        for {
          actual <- eval(tupleTv)
        } yield assertTrue(actual == expected)
      }
    }

  def tupleArityCases2to22 = Seq(
    case2,
    case3,
    case4,
    case5,
    case6,
    case7,
    case8,
    case9,
    case10,
    case11,
    case12,
    case13,
    case14,
    case15,
    case16,
    case17,
    case18,
    case19,
    case20,
    case21,
    case22
  )

  def name(i: Int) = s"element$i"

  def element(i: Int) = V.string(name(i)) -> ir.sdk.String.stringType

  val case2 = (
    "Should be possible to evaluate a tuple of arity 2",
    V.tuple(element(1), element(2)),
    (name(1), name(2))
  )

  val case3 = (
    "Should be possible to evaluate a tuple of arity 3",
    V.tuple(element(1), element(2), element(3)),
    (name(1), name(2), name(3))
  )

  val case4 = (
    "Should be possible to evaluate a tuple of arity 4",
    V.tuple(element(1), element(2), element(3), element(4)),
    (name(1), name(2), name(3), name(4))
  )

  val case5 = (
    "Should be possible to evaluate a tuple of arity 5",
    V.tuple(element(1), element(2), element(3), element(4), element(5)),
    (name(1), name(2), name(3), name(4), name(5))
  )
  val case6 = (
    "Should be possible to evaluate a tuple of arity 6",
    V.tuple(element(1), element(2), element(3), element(4), element(5), element(6)),
    (name(1), name(2), name(3), name(4), name(5), name(6))
  )

  val case7 = (
    "Should be possible to evaluate a tuple of arity 7",
    V.tuple(element(1), element(2), element(3), element(4), element(5), element(6), element(7)),
    (name(1), name(2), name(3), name(4), name(5), name(6), name(7))
  )

  val case8 = (
    "Should be possible to evaluate a tuple of arity 8",
    V.tuple(element(1), element(2), element(3), element(4), element(5), element(6), element(7), element(8)),
    (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8))
  )

  val case9 = (
    "Should be possible to evaluate a tuple of arity 9",
    V.tuple(
      element(1),
      element(2),
      element(3),
      element(4),
      element(5),
      element(6),
      element(7),
      element(8),
      element(9)
    ),
    (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9))
  )

  val case10 = (
    "Should be possible to evaluate a tuple of arity 10",
    V.tuple(
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
    ),
    (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9), name(10))
  )

  val case11 = (
    "Should be possible to evaluate a tuple of arity 11",
    V.tuple(
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
    ),
    (name(1), name(2), name(3), name(4), name(5), name(6), name(7), name(8), name(9), name(10), name(11))
  )

  val case12 = (
    "Should be possible to evaluate a tuple of arity 12",
    V.tuple(
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
    ),
    (
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
      name(12)
    )
  )

  val case13 = (
    "Should be possible to evaluate a tuple of arity 13",
    V.tuple(
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
    ),
    (
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
  )

  val case14 = (
    "Should be possible to evaluate a tuple of arity 14",
    V.tuple(
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
    ),
    (
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
  )

  val case15 = (
    "Should be possible to evaluate a tuple of arity 15",
    V.tuple(
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
    ),
    (
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
  )

  val case16 = (
    "Should be possible to evaluate a tuple of arity 16",
    V.tuple(
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
    ),
    (
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
  )

  val case17 = (
    "Should be possible to evaluate a tuple of arity 17",
    V.tuple(
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
    ),
    (
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
  )

  val case18 = (
    "Should be possible to evaluate a tuple of arity 18",
    V.tuple(
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
    ),
    (
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
  )

  val case19 = (
    "Should be possible to evaluate a tuple of arity 19",
    V.tuple(
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
    ),
    (
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
  )

  val case20 = (
    "Should be possible to evaluate a tuple of arity 20",
    V.tuple(
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
    ),
    (
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
  )

  val case21 = (
    "Should be possible to evaluate a tuple of arity 21",
    V.tuple(
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
    ),
    (
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
  )

  val case22 = (
    "Should be possible to evaluate a tuple of arity 22",
    V.tuple(
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
    ),
    (
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
  )

  val negativeCase23 = test("Should not evaluate tuples of arity 23") {
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
    for {
      actual <- eval(value).either
    } yield assertTrue(actual.isLeft)
  }
}

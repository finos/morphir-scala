package org.finos.morphir.runtime.quick

import org.finos.morphir.runtime.IllegalValue

sealed trait TupleSigniture[TA, VA] {
  def value: Product
  def toList = value.productIterator.map(_.asInstanceOf[Result[TA, VA]]).toList
}
object TupleSigniture {
  def fromList[TA, VA](list: List[Result[TA, VA]]) =
    // Large tuple signatures will overflow the formatting limitations, just need for this part of the file
    // format: off
    list match {
      case List(a) => Tup1[TA, VA](Tuple1(a))
      case List(a, b) => Tup2[TA, VA]((a, b))
      case List(a, b, c) => Tup3[TA, VA]((a, b, c))
      case List(a, b, c, d) => Tup4[TA, VA]((a, b, c, d))
      case List(a, b, c, d, e) => Tup5[TA, VA]((a, b, c, d, e))
      case List(a, b, c, d, e, f) => Tup6[TA, VA]((a, b, c, d, e, f))
      case List(a, b, c, d, e, f, g) => Tup7[TA, VA]((a, b, c, d, e, f, g))
      case List(a, b, c, d, e, f, g, h) => Tup8[TA, VA]((a, b, c, d, e, f, g, h))
      case List(a, b, c, d, e, f, g, h, i) => Tup9[TA, VA]((a, b, c, d, e, f, g, h, i))
      case List(a, b, c, d, e, f, g, h, i, j) => Tup10[TA, VA]((a, b, c, d, e, f, g, h, i, j))
      case List(a, b, c, d, e, f, g, h, i, j, k) => Tup11[TA, VA]((a, b, c, d, e, f, g, h, i, j, k))
      case List(a, b, c, d, e, f, g, h, i, j, k, l) => Tup12[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m) => Tup13[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => Tup14[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => Tup15[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => Tup16[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => Tup17[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => Tup18[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => Tup19[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => Tup20[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => Tup21[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => Tup22[TA, VA]((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
      case _ => throw new IllegalValue(
          s"Cannot convert a list of length ${list.length} into a tuple, it is too big (the first elements of the list are [${printListUpTo10(list)}])."
        )
    }
    // format: on

  private def printListUpTo10[T](list: List[T]) = {
    val dots = if (list.length > 10) "..." else ""
    list.take(10).mkString("", ", ", dots)
  }

  // Large tuple signatures will overflow the formatting limitations, just need for this part of the file
  // format: off
  case class Tup1[TA, VA](value: Tuple1[Result[TA, VA]]) extends TupleSigniture[TA, VA]
  case class Tup2[TA, VA](value: (Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup3[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup4[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup5[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup6[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup7[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup8[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup9[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup10[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup11[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup12[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup13[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup14[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup15[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup16[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup17[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup18[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup19[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup20[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup21[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
  case class Tup22[TA, VA](value: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA])) extends TupleSigniture[TA, VA]
// format: off
}

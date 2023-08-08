package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Literal.Literal.{
  BoolLiteral,
  CharLiteral,
  DecimalLiteral,
  FloatLiteral,
  StringLiteral,
  WholeNumberLiteral
}
import org.finos.morphir.ir.{Value => V}
import org.finos.morphir.ir.Value.Pattern.*

object Helpers {

  def unpackLit(literal: Lit): Any = literal match {
    case StringLiteral(value)      => value
    case FloatLiteral(value)       => value
    case CharLiteral(value)        => value
    case BoolLiteral(value)        => value
    case WholeNumberLiteral(value) => value
    case DecimalLiteral(value)     => value
  }

  def matchPatternCase[TA, VA](
      pattern: V.Pattern[VA],
      value: Result[TA, VA]
  ): Option[Map[Name, Result[TA, VA]]] =
    (pattern, value) match {
      case (_: WildcardPattern[VA], _) => Some(Map.empty)
      case (AsPattern(_, innerPattern, name), innerValue) =>
        matchPatternCase(innerPattern, value).map(innerBindings => innerBindings + (name -> innerValue))
      case (_: UnitPattern[VA], Result.Unit()) => Some(Map.empty)
      case (LiteralPattern(_, literal), Result.Primitive(innerValue)) if unpackLit(literal) == innerValue =>
        Some(Map.empty)
      case (_: EmptyListPattern[VA], Result.ListResult(List())) => Some(Map.empty)
      case (HeadTailPattern(_, headPattern, tailPattern), Result.ListResult(head :: tail)) =>
        for {
          headBindings <- matchPatternCase(headPattern, head)
          tailBindings <- matchPatternCase(tailPattern, Result.ListResult(tail))
        } yield headBindings ++ tailBindings
      case (TuplePattern(_, patterns), Result.Tuple(tuple)) =>
        for {
          listedTuple: List[Result[TA, VA]] <- tupleToList[TA, VA](tuple)
          res                               <- matchListOfPatterns(patterns.toList, listedTuple)
        } yield res
      case (ConstructorPattern(_, patternName, patterns), Result.ConstructorResult(valueName, values))
          if patternName == valueName =>
        matchListOfPatterns(patterns.toList, values)
      // TODO: Destructure records
      case _ => None
    }

  private def matchListOfPatterns[TA, VA](
      patterns: List[V.Pattern[VA]],
      values: List[Result[TA, VA]]
  ): Option[Map[Name, Result[TA, VA]]] =
    for {
      zipped <- if (patterns.length == values.length) Some(patterns.zip(values)) else None
      res <- zipped.foldLeft(Some(Map.empty): Option[Map[Name, Result[TA, VA]]]) { case (bindings, (p, v)) =>
        for {
          priorBindings <- bindings
          newBindings   <- matchPatternCase(p, v)
        } yield priorBindings ++ newBindings
      }
    } yield res

  def tupleToList[TA, VA](tuple: Any): Option[List[Result[TA, VA]]] = {
    val anyList = tuple match {
      case Tuple1(a)                                     => Some(List(a))
      case (a, b)                                        => Some(List(a, b))
      case (a, b, c)                                     => Some(List(a, b, c))
      case (a, b, c, d)                                  => Some(List(a, b, c, d))
      case (a, b, c, d, e)                               => Some(List(a, b, c, d, e))
      case (a, b, c, d, e, f)                            => Some(List(a, b, c, d, e, f))
      case (a, b, c, d, e, f, g)                         => Some(List(a, b, c, d, e, f, g))
      case (a, b, c, d, e, f, g, h)                      => Some(List(a, b, c, d, e, f, g, h))
      case (a, b, c, d, e, f, g, h, i)                   => Some(List(a, b, c, d, e, f, g, h, i))
      case (a, b, c, d, e, f, g, h, i, j)                => Some(List(a, b, c, d, e, f, g, h, i, j))
      case (a, b, c, d, e, f, g, h, i, j, k)             => Some(List(a, b, c, d, e, f, g, h, i, j, k))
      case (a, b, c, d, e, f, g, h, i, j, k, l)          => Some(List(a, b, c, d, e, f, g, h, i, j, k, l))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m)       => Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n)    => Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
        Some(List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
      case _ => None
    }
    anyList.map(_.map(_.asInstanceOf[Result[TA, VA]]))
  }

  def listToTuple(
      items: List[Any]
  ): Any =
    items match {
      case List(a, b)                                     => (a, b)
      case List(a, b, c)                                  => (a, b, c)
      case List(a, b, c, d)                               => (a, b, c, d)
      case List(a, b, c, d, e)                            => (a, b, c, d, e)
      case List(a, b, c, d, e, f)                         => (a, b, c, d, e, f)
      case List(a, b, c, d, e, f, g)                      => (a, b, c, d, e, f, g)
      case List(a, b, c, d, e, f, g, h)                   => (a, b, c, d, e, f, g, h)
      case List(a, b, c, d, e, f, g, h, i)                => (a, b, c, d, e, f, g, h, i)
      case List(a, b, c, d, e, f, g, h, i, j)             => (a, b, c, d, e, f, g, h, i, j)
      case List(a, b, c, d, e, f, g, h, i, j, k)          => (a, b, c, d, e, f, g, h, i, j, k)
      case List(a, b, c, d, e, f, g, h, i, j, k, l)       => (a, b, c, d, e, f, g, h, i, j, k, l)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m)    => (a, b, c, d, e, f, g, h, i, j, k, l, m)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
      case _ => throw new Exception(s"Unsuported tuple arity ${items.size}")
    }

}

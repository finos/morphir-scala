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
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.ir.Type.UType

object Helpers {

  def unpackLit(literal: Lit): RTValue.Primitive[_] = literal match {
    case StringLiteral(value)      => RTValue.Primitive.String(value)
    case FloatLiteral(value)       => RTValue.Primitive.Float(value)
    case CharLiteral(value)        => RTValue.Primitive.Char(value)
    case BoolLiteral(value)        => RTValue.Primitive.Boolean(value)
    case WholeNumberLiteral(value) => RTValue.Primitive.Int(MInt.fromLong(value))
    case DecimalLiteral(value)     => RTValue.Primitive.BigDecimal(value)
  }

  def matchPatternCase(
      pattern: V.Pattern[UType],
      value: RTValue
  ): Option[Map[Name, RTValue]] =
    (pattern, value) match {
      case (_: WildcardPattern[UType], _) => Some(Map.empty)
      case (AsPattern(_, innerPattern, name), innerValue) =>
        matchPatternCase(innerPattern, value).map(innerBindings => innerBindings + (name -> innerValue))
      case (_: UnitPattern[UType], RTValue.Unit()) => Some(Map.empty)
      case (LiteralPattern(_, literal), innerValue: RTValue.Primitive[_]) if unpackLit(literal) == innerValue =>
        Some(Map.empty)
      case (_: EmptyListPattern[UType], RTValue.List(List())) => Some(Map.empty)
      case (HeadTailPattern(_, headPattern, tailPattern), RTValue.List(head :: tail)) =>
        for {
          headBindings <- matchPatternCase(headPattern, head)
          tailBindings <- matchPatternCase(tailPattern, RTValue.List(tail))
        } yield headBindings ++ tailBindings
      case (TuplePattern(_, patterns), RTValue.Tuple(tuple)) =>
        matchListOfPatterns(patterns.toList, tuple.toList)
      case (ConstructorPattern(_, patternName, patterns), RTValue.ConstructorResult(valueName, values))
          if patternName == valueName =>
        matchListOfPatterns(patterns.toList, values)
      // TODO: Destructure records
      case _ => None
    }

  private def matchListOfPatterns(
      patterns: List[V.Pattern[UType]],
      values: List[RTValue]
  ): Option[Map[Name, RTValue]] =
    for {
      zipped <- if (patterns.length == values.length) Some(patterns.zip(values)) else None
      res <- zipped.foldLeft(Some(Map.empty): Option[Map[Name, RTValue]]) { case (bindings, (p, v)) =>
        for {
          priorBindings <- bindings
          newBindings   <- matchPatternCase(p, v)
        } yield priorBindings ++ newBindings
      }
    } yield res

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

package org.finos.morphir.lang

import org.finos.morphir.core.internal.Visitor

trait Readable:
  type TypeAttribs
  type ValueAttribs
  def transform[T](f: Visitor[TypeAttribs, ValueAttribs, _, T]): T

object Readable:
  type WithAttribs[TA, VA] = Readable {
    type TypeAttribs  = TA
    type ValueAttribs = VA
  }
  case class fromTransformer[TA, VA, T](t: T, w: Transformer.WithAttribs[TA, VA, T]) extends Readable:
    final override type TypeAttribs  = TA
    final override type ValueAttribs = VA

    def transform[T](f: Visitor[TypeAttribs, ValueAttribs, _, T]): T =
      w.transform(t, f)

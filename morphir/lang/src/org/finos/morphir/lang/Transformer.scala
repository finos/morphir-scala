package org.finos.morphir.lang

import org.finos.morphir.core.internal.Visitor

trait Transformer[I]:
  type TypeAttribs
  type ValueAttribs
  def transform[T](input: I, f: Visitor[TypeAttribs, ValueAttribs, _, T]): T

object Transformer:
  type WithAttribs[TA, VA, T] = Transformer[T] {
    type TypeAttribs  = TA
    type ValueAttribs = VA
  }

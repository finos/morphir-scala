package org.finos.morphir.ir

import upickle.core.Visitor

trait Readable {
  def transform[T](v: Visitor[_, T]): T
}

object Readable {
  case class fromTransformer[T](t: T, w: Transformer[T]) extends Readable {
    def transform[T](f: Visitor[_, T]): T =
      w.transform(t, f)
  }
}

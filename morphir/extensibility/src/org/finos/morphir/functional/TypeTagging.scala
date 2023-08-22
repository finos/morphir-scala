package org.finos.morphir.functional

object TypeTagging {
  type @@[T, Tag] = T with Tag

  def tag[T, Tag](t: T): T @@ Tag = t.asInstanceOf[T @@ Tag]
}

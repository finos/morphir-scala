package org.finos.morphir.core

trait Types extends CoreModule:
  types =>

  trait Reader[T] extends Visitor[Any, T]:
    self =>
    def narrow[K <: T]: Reader[K] = self.asInstanceOf[Reader[K]]

  trait Writer[T]:
    self =>
    def write0[V](out: Visitor[_, V], v: T): V
    def write[V](out: Visitor[_, V], v: T): V =
      if (v == null) out.fail(-1)
      else write0(out, v)
  end Writer

end Types

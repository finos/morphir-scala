package org.finos
package morphir
package core

trait Types extends CoreModule {
  types =>

  trait SimpleReader[T] extends Reader[T] with SimpleVisitor[Any, T]

  trait Reader[T] extends Visitor[Any, T] { self =>
    def narrow[K <: T]: Reader[K] = self.asInstanceOf[Reader[K]]
  }

  object Reader {
    abstract class MapReader[-In, V, Z](delegatedReader: Visitor[In, V])
        extends Visitor.MapReader[TypeAttribs, ValueAttribs, In, V, Z](delegatedReader)
        with Reader[Z]
  }

  trait Writer[T] {
    self =>
    def write0[V](out: Visitor[_, V], v: T): V

    def write[V](out: Visitor[_, V], v: T): V =
      if (v == null) out.visitNull(-1)
      else write0(out, v)
  }

}

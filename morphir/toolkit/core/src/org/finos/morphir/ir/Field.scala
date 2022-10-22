package org.finos.morphir
package ir

import org.finos.morphir.ir.Name
import zio.prelude._

final case class Field[+T](name: Name, data: T) { self =>

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(data))

}

object Field {

  def apply[T](name: String, data: T): Field[T] = Field(Name.fromString(name), data)

  type Untyped = Field[Unit]
  object Untyped {
    def apply(name: Name): Field[Unit]    = Field(name, ())
    def unapply(field: Field[Unit]): Name = field.name
  }
}

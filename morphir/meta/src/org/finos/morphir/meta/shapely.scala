// This file was taken from the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta
/// An isomorphism (up to primitive boxing and label names) between user-defined
/// data types and a Shape.
///
/// Intentionally restricted to Shapes to avoid the rabbit hole of being a
/// typeclass capturing the concept of an isomorphism. As such, there are no laws
/// pertaining to associativity because, by convention, a Shape should not appear
/// in the A position and a B should always be a Shape. The former cannot be
/// enforced in the type system and the latter is not enforced since it is too
/// much for Scala 2.12 and below (specifically, the macros fail to produce the
/// required evidence).
///
/// Laws:
///
/// - identity: to(from(b)) == b AND from(to(a)) == a
trait Shapely[A, B] {
  def to(a: A): B
  def from(b: B): A
}

object Shapely extends ShapelyCompat {
  def apply[A, B](implicit S: Shapely[A, B]): Shapely[A, B] = S
}

// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

// Workaround the lack of by-name implicit parameters in Scala 2.12 and lower by
// using a `Lazy[A]` instead of `=> A` whilst also adding memoisation, giving a
// reason to use it in 2.13+.
//
// by-name implicit parameters are necessary in tactical positions, such as the
// dependencies of a CaseClass or SealedTrait derivation, to avoid null pointer
// exceptions at runtime between mutually dependent rules, or infinite recursion
// between multiple lazy implicit values. A common failure is a sealed trait
// where one of the case classes depends on the sealed trait itself (a recursive
// type), possibly via another parameterised type (List is common). In such
// cases, adding the `lazy` keyword to the `implicit val` on the sealed trait,
// while using `def` on the case classes, can also help reduce the risk of
// cycles resulting in NPE. See the shapely tests for examples.
final class Lazy[A] private (private[this] var eval: () => A) {
  lazy val value: A = {
    val value0 = eval()
    eval = null
    value0
  }
}

object Lazy extends LazyCompat {
  def apply[A](a: => A): Lazy[A] = new Lazy[A](() => a)
}

// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

trait LazyCompat { this: Lazy.type =>
  implicit def gen[A](implicit A: => A): Lazy[A] = Lazy(A)
}

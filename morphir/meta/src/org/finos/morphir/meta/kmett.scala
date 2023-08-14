// Inspired by Kmett's work in Scalaz and Haskell
//
// Introduces core typeclasses: Decide, Align, XFunctor (DAX), which are
// specialised into Align, Covariant, Decide, Contravariant (AC/DC).
//
// For further background on Kmett's work in Scalaz, see
// https://leanpub.com/fpmortals/read#leanpub-auto-typeclass-derivation and
// especially Scalaz typeclasses Invariant, Contravariant, Divisible, Apply,
// Alt, and Decidable.
//
// Note that all APIs use by-name parameters. This is to workaround
// initialisation problems during implicit resolution for mutually referential
// (e.g. recursive) data types. Implementors should consider caching parameters
// in lazy vals.
// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

// Mixin for typeclass companions to activate `derived` instances for case
// classes and sealed traits using the DAX mechanism.
//
// Align provides support for case classes.
// Decide provides support for sealed traits.
//
// In both cases, an XFunctor must be available.
trait Derivable[F[_]] extends DerivableGenerated[F] {
  def derived[A, B](implicit X: XFunctor[F], S: Shapely[A, B], B: Lazy[F[B]]): F[A] = X.xmap(B.value)(S.from, S.to)

  // caseclass0 is not automatically provided (supporting case objects and
  // parameterless case classes). It is roughly equivalent to Divisible.conquer,
  //
  // implicit def caseclass0[A]: F[CaseClass0[A]] = ...
}

// c.f. InvariantFunctor
//
// Laws:
//
// - identity: `fa == xmap(fa)(id, id)`
// - composition: `xmap(xmap(fa, f1, g1), f2, g2) == xmap(fa, f2 . f1, g1 . g2)`
trait XFunctor[F[_]] {
  def xmap[A, B](fa: => F[A])(f: A => B, g: B => A): F[B]
}

// Laws:
//
// - identity: `fa == fmap(fa)(id)`
// - composition: `fmap(fmap(fa, f1), f2) == fmap(fa, f2 . f1)`
trait Covariant[F[_]] extends XFunctor[F] {
  final def widen[A, B](fa: => F[A])(implicit E: A <:< B): F[B]          = fmap(fa)(E.apply)
  final override def xmap[A, B](fa: => F[A])(f: A => B, g: B => A): F[B] = fmap(fa)(f)
  def fmap[A, B](fa: => F[A])(f: A => B): F[B]
}

// Laws:
//
// - identity: `fa == contramap(fa)(id)`
// - composition: `contramap(contramap(fa, f1), f2) == contramap(fa, f2 . f1)`
trait Contravariant[F[_]] extends XFunctor[F] {
  final def narrow[A, B](fa: => F[B])(implicit E: A <:< B): F[A]         = contramap(fa)(E.apply)
  final override def xmap[A, B](fa: => F[A])(f: A => B, g: B => A): F[B] = contramap(fa)(g)
  def contramap[A, B](fa: => F[A])(f: B => A): F[B]
}

// c.f. Applicative / Divisible
//
// Laws (up to unwrapped tuples):
//
// - associativity: `align(align(fa, fb), fc) == align(fa, align(fb, fc))`
trait Align[F[_]] {
  def align[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
}

// c.f. Decidable / Alt
//
// Laws (up to unwrapped eithers):
//
// - associativity: `decide(decide(fa, fb), fc) == decide(fa, decide(fb, fc))`
trait Decide[F[_]] {
  def decide[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]]
}

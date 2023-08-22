package org.finos.morphir.meta

import scala.annotation.nowarn

object ShapelyTestExamples {
  sealed trait Gaz
  case class Foo(s: String)              extends Gaz
  case class Bar(txt: String, num: Long) extends Gaz
  case class Baz()                       extends Gaz
  case object Car                        extends Gaz

  sealed trait Poly[+A]
  case class PolyFoo(s: String, i: Int) extends Poly[Int]
  case class PolyBar()                  extends Poly[Unit]

  sealed abstract class ATree
  final case class Leaf(value: String)        extends ATree
  final case class Branch(roots: List[ATree]) extends ATree

  final case class lower(foo: String)

  // a typeclass that doesn't do anything, but shows how Shapely can be used to
  // derived typeclasses from generated boilerplate.
  trait Nuthin[A] { self =>
    def xmap[B](f: A => B, g: B => A): Nuthin[B] = self.asInstanceOf[Nuthin[B]]
  }
  object Nuthin {
    def derived[A, B](implicit S: Shapely[A, B], B: Nuthin[B]): Nuthin[A] = B.xmap(S.from, S.to)
    private def nuthin[A]                                                 = new Nuthin[A] {}

    implicit val string: Nuthin[String] = nuthin

    implicit def list[A](implicit A: Nuthin[A]): Nuthin[List[A]] = {
      assert(A != null)
      nuthin
    }

    // the typeclass author would typically generate the following for all arities of case class and sealed trait
    @nowarn
    implicit def caseclass1[A, A1](implicit M: Meta[A], A1: Lazy[Nuthin[A1]]): Nuthin[CaseClass1[A, A1]] = {
      assert(A1.value != null)
      nuthin
    }

    @nowarn
    implicit def sealedtrait2[A, A1 <: A, A2 <: A](implicit
        M: Meta[A],
        M1: Meta[A1],
        A1: Lazy[Nuthin[A1]],
        M2: Meta[A2],
        A2: Lazy[Nuthin[A2]]
    ): Nuthin[SealedTrait2[A, A1, A2]] = {
      assert(A1.value != null && A2.value != null)
      nuthin
    }
  }

  // object ATree {
  //   implicit lazy val nuthin: Nuthin[ATree] = Nuthin.derived
  // }
  // object Leaf {
  //   implicit def nuthin: Nuthin[Leaf] = Nuthin.derived
  // }
  // object Branch {
  //   implicit def nuthin: Nuthin[Branch] = Nuthin.derived
  // }
  object ATree {
    implicit lazy val nuthin: Nuthin[ATree] = {
      implicit def leaf: Nuthin[Leaf]     = Nuthin.derived
      implicit def branch: Nuthin[Branch] = Nuthin.derived
      Nuthin.derived
    }
  }

}

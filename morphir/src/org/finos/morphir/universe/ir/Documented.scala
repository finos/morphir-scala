package org.finos.morphir.universe.ir

import zio.prelude.*
import Documented.Doc

final case class Documented[+A](doc: String, value: A) {
  def map[B](f: A => B): Documented[B]                 = Documented(doc, f(value))
  def flatMap[B](f: A => Documented[B]): Documented[B] = f(value)
}

object Documented {
  import org.typelevel.paiges.{Doc => doc, *}

  def fromDoc[A](doc: Doc, value: A, width: Int = 120): Documented[A] = Documented(doc.render(width), value)

  type Doc = doc
  object Doc extends Newtype[doc] {
    def apply(text: String): Doc = doc.text(text)
  }
  implicit val DocumentedCovariant: Covariant[Documented] = new Covariant[Documented] {
    def map[A, B](f: A => B): Documented[A] => Documented[B] = _.map(f)
  }

  implicit val DocumentedForEach: ForEach[Documented] =
    new ForEach[Documented] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](self: Documented[A])(f: A => G[B]): G[Documented[B]] =
        f(self.value).map(Documented(self.doc, _))
    }
}

package org.finos.morphir.core.types
import Documented.*
import monix.newtypes.*

/**
 * Type that represents a documented value
 */
final case class Documented[+A](doc: Doc, value: A) {
  def map[B](f: A => B): Documented[B] = Documented(doc, f(value))
}

object Documented {
  import org.typelevel.paiges.{Doc => doc, *}

  type Doc = doc
  object Doc extends NewtypeWrapped[doc] {
    def apply(text: String): Doc = doc.text(text)
  }
}

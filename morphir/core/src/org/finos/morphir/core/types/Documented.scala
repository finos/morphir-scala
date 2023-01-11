package org.finos
package morphir
package core.types

import Documented.*
import morphir.prelude.*

/**
 * Type that represents a documented value
 */
final case class Documented[+A](doc: Doc, value: A) {
  def map[B](f: A => B): Documented[B] = Documented(doc, f(value))
}

object Documented {
  import org.typelevel.paiges.{Doc => doc, *}

  type Doc = doc
  object Doc extends Newtype[doc] {
    def apply(text: String): Doc = doc.text(text)
  }
}

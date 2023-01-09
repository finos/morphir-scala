package org.finos.morphir.core.types
import Documented.*

/**
 * Type that represents a documented value
 */
final case class Documented[+A](doc: Doc, value: A):
  def map[B](f: A => B): Documented[B] = Documented(doc, f(value))

object Documented:
  import org.typelevel.paiges.{Doc => doc, *}
  opaque type Doc = doc
  object Doc:
    def apply(text: String): Doc = doc.text(text)

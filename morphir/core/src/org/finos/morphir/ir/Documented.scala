package org.finos.morphir.ir
import org.finos.morphir.foundations.*
object Documented {
  import Documented.Doc
  final case class Documented[+A](doc: String, value: A) {
    def map[B](f: A => B): Documented[B] = Documented(doc, f(value))
  }

  object Documented {
    import org.typelevel.paiges.{Doc => doc, *}

    def fromDoc[A](doc: Doc, value: A, width: Int = 120): Documented[A] = Documented(doc.render(width), value)

    type Doc = doc
    object Doc extends Newtype[doc] {
      def apply(text: String): Doc = doc.text(text)
    }
  }

}

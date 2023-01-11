package org.finos.morphir.core.types
import monix.newtypes.*
object Strings {
  type EncodedString = EncodedString.Type
  object EncodedString extends NewsubtypeWrapped[String] {
    def encode(input: CharSequence)(implicit encoder: StringEncoder): EncodedString =
      EncodedString(encoder.encode(input))

    def encodeWith(encoder: CharSequence => String)(input: CharSequence): EncodedString =
      EncodedString(encoder(input))

    def unapply(input: CharSequence): Option[String] =
      input match {
        case EncodedString(s) => Option(s)
        case _                => None
      }
  }
  trait StringEncoder {
    def encode(input: CharSequence): String
  }
}

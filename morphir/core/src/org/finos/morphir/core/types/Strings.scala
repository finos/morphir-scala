package org.finos
package morphir
package core.types

import morphir.prelude._
object Strings {
  type EncodedString = EncodedString.Type
  object EncodedString extends Newtype[String] {
    def encode(input: CharSequence)(implicit encoder: StringEncoder): EncodedString =
      EncodedString(encoder.encode(input))

    def encodeWith(encoder: CharSequence => String)(input: CharSequence): EncodedString =
      EncodedString(encoder(input))

    def unapply(input: CharSequence): Option[String] =
      input match {
        case EncodedString(s) => Option(s)
        case _                => None
      }

    implicit class EncodedStringOps(val self: EncodedString) extends AnyVal {
      def value: String = unwrap(self)
    }
  }
  trait StringEncoder {
    def encode(input: CharSequence): String
  }
}

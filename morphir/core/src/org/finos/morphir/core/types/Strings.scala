package org.finos.morphir.core.types

object Strings:
  opaque type EncodedString <: String = String
  object EncodedString:
    def encode(input: CharSequence)(using encoder: StringEncoder): EncodedString =
      encoder.encode(input)

    def encodeWith(encoder: CharSequence => String)(input: CharSequence): EncodedString =
      encoder(input)

    def unapply(input: CharSequence): Option[String] =
      input match
        case s: EncodedString => Option(s)
        case _                => None

  trait StringEncoder:
    def encode(input: CharSequence): String

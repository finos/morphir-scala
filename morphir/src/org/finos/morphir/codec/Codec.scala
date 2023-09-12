package org.finos.morphir.codec

trait Codec[Whole, Element, A] extends Encoder[Whole, Element, A] with Decoder[Whole, Element, A]

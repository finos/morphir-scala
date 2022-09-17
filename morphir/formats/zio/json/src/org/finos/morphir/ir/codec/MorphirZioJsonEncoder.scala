package org.finos.morphir.ir.codec
import org.finos.morphir.ir.codec.encoders.MorphirJsonEncoder
import zio.json._

class MorphirZioJsonEncoder[-TAttr, -VAttr](using JsonEncoder[TAttr], JsonEncoder[VAttr])
    extends MorphirJsonEncoder[TAttr, VAttr]:
  given JsonEncoder[Unit]                               = JsonEncoder.array[Int].contramap(_ => Array.empty)
  def encodeTypeAttributes(input: TAttr): CharSequence  = input.toJson
  def encodeValueAttributes(input: VAttr): CharSequence = input.toJson
  def encodeString(input: String): CharSequence         = input.toJson
  def encodeUnit(input: Unit): CharSequence             = input.toJson

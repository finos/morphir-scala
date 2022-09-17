package org.finos.morphir.ir.codec

import org.finos.morphir.formats.errors.DecodingError
import org.finos.morphir.formats.json.Json

object encoders:
  trait MorphirEncoder[-TAttr, -VAttr, +A]:
    def encodeTypeAttributes(input: TAttr): A
    def encodeValueAttributes(input: VAttr): A
    def encodeString(input: String): A
    def encodeUnit(input: scala.Unit): A

  trait MorphirJsonEncoder[-TAttr, -VAttr] extends MorphirEncoder[TAttr, VAttr, CharSequence]:
    def encodeJson(json: Json, indent: Int = 0): CharSequence

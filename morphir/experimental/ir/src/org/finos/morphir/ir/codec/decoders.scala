package org.finos.morphir.ir.codec
import org.finos.morphir.formats.errors.DecodingError

object decoders:
  trait MorphirDecoder[-Input, +TAttr, +VAttr]:
    def asString(input: Input): Either[DecodingError, String]

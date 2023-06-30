package org.finos.morphir.datamodel

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import org.finos.morphir.ir.sdk.{Basics, String as StringModule}
import org.finos.morphir.datamodel.DataDecoder.DecodingError
sealed trait IRDataDecoder {
  implicit val typedValue:DataDecoder[TypedValue] = (data:Data) => data match {
    case Data.Boolean(value) => Right(V.boolean(Basics.boolType, value))
    case Data.String(value) => Right(V.string(StringModule.stringType, value))
    case _ => Left(DecodingError.InvalidConversion("TypedValue"))
  }
}

object IRDataDecoder extends IRDataDecoder

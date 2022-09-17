package org.finos.morphir.ir.zio.json
import org.finos.morphir.ir.codec.JsonFormatVersion
import zio.json._

trait Encoders(using formatVersion: JsonFormatVersion):
  given JsonEncoder[scala.Unit] = JsonEncoder.array[Int].contramap(_ => Array.empty)

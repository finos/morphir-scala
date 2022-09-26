package org.finos.morphir.ir.zio.json
import org.finos.morphir.ir.codec.JsonFormatVersion
import zio.json._

trait Encoders(using formatVersion: JsonFormatVersion):
  given JsonEncoder[JsonFormatVersion] = JsonEncoder.int.contramap(_.value)
  given JsonEncoder[scala.Unit]        = JsonEncoder.array[Int].contramap(_ => Array.empty)

object Encoders:
  object V1 extends Encoders(using JsonFormatVersion.V1)
  object V2 extends Encoders(using JsonFormatVersion.V2)

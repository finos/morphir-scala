package org.finos.morphir.ir.json.codec

import org.finos.morphir.ir._
import zio.json._
import NameJsonCodecProvider._

object PathJsonCodecProvider extends PathJsonCodecProvider

trait PathJsonCodecProvider {
  implicit val pathEncoder: JsonEncoder[Path] = JsonEncoder.list[Name].contramap(path => path.segments.toList)
  implicit val pathDecoder: JsonDecoder[Path] = JsonDecoder.list[Name].map(Path.fromList)
}

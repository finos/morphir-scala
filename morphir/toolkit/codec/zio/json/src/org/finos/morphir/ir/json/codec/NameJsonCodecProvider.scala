package org.finos.morphir
package ir
package json
package codec

import zio.json._

trait NameJsonCodecProvider {
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val nameDecoder: JsonDecoder[Name] = JsonDecoder.list[String].map(Name.fromList)
}

object NameJsonCodecProvider extends NameJsonCodecProvider

package org.finos.morphir.dataformat.mdf.json.zio

import org.finos.morphir.dataformat.mdf._
import zio.json._

trait ZioJsonBaseEncoders {
  implicit val LabelEncoder: JsonEncoder[Label] = DeriveJsonEncoder.gen[Label]
}

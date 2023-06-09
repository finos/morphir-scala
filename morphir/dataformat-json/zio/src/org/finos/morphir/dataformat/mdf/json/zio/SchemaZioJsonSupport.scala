package org.finos.morphir.dataformat.mdf.json.zio

import org.finos.morphir.dataformat.mdf._
import zio.json._

trait SchemaZioJsonSupport extends SchemaZioJsonEncoders {}

trait SchemaZioJsonEncoders extends ZioJsonBaseEncoders {
  implicit val EnumCaseField: JsonEncoder[Schema.Enum.Case.Field] = DeriveJsonEncoder.gen[Schema.Enum.Case.Field]
  implicit val EnumCaseEncoder: JsonEncoder[Schema.Enum.Case]     = DeriveJsonEncoder.gen[Schema.Enum.Case]
  implicit val SchemaAliasEncoder: JsonEncoder[Schema.Alias]      = DeriveJsonEncoder.gen[Schema.Alias]
  implicit val SchemaJsonEncoder: JsonEncoder[Schema]             = DeriveJsonEncoder.gen[Schema]
}

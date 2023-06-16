package org.finos.morphir.datamodel.json.zio

import org.finos.morphir.datamodel._
import zio.json._

object codecs extends SchemaZioJsonSupport with DataZioJsonSupport

trait ZioJsonBaseEncoders {
  implicit val LabelEncoder: JsonEncoder[Label]                    = DeriveJsonEncoder.gen[Label]
  implicit val BasicDataTypeEncoder: JsonEncoder[BasicDataType[_]] = DeriveJsonEncoder.gen[BasicDataType[_]]
}

trait SchemaZioJsonSupport extends SchemaZioJsonEncoders {}

trait SchemaZioJsonEncoders extends ZioJsonBaseEncoders {
  implicit val EnumCaseField: JsonEncoder[Concept.Enum.Case.Field] = DeriveJsonEncoder.gen[Concept.Enum.Case.Field]
  implicit val EnumCaseEncoder: JsonEncoder[Concept.Enum.Case]     = DeriveJsonEncoder.gen[Concept.Enum.Case]
  implicit val SchemaAliasEncoder: JsonEncoder[Concept.Alias]      = DeriveJsonEncoder.gen[Concept.Alias]
  implicit val SchemaJsonEncoder: JsonEncoder[Concept]             = DeriveJsonEncoder.gen[Concept]
}

trait DataZioJsonSupport extends DataZioJsonEncoders { self: SchemaZioJsonSupport => }

trait DataZioJsonEncoders extends ZioJsonBaseEncoders { self: SchemaZioJsonEncoders =>
  // implicit val DataJsonEncoder:JsonEncoder[Data] = DeriveJsonEncoder.gen[Data]
}

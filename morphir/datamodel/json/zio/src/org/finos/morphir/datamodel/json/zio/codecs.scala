package org.finos.morphir.datamodel.json.zio

import org.finos.morphir.datamodel.*
import org.finos.morphir.datamodel.namespacing.QualifiedName
import zio.json.*

object codecs extends SchemaZioJsonSupport with DataZioJsonSupport

trait ZioJsonBaseEncoders {
  implicit val LabelEncoder: JsonEncoder[Label]                    = DeriveJsonEncoder.gen[Label]
  implicit val EnumLabelEncoder: JsonEncoder[EnumLabel]            = DeriveJsonEncoder.gen[EnumLabel]
  implicit val BasicDataTypeEncoder: JsonEncoder[BasicDataType[_]] = DeriveJsonEncoder.gen[BasicDataType[_]]
}

trait SchemaZioJsonSupport extends SchemaZioJsonEncoders {}

trait SchemaZioJsonEncoders extends ZioJsonBaseEncoders {
  implicit val EnumCaseEncoder: JsonEncoder[Concept.Enum.Case] = DeriveJsonEncoder.gen[Concept.Enum.Case]
  implicit val SchemaAliasEncoder: JsonEncoder[Concept.Alias]  = DeriveJsonEncoder.gen[Concept.Alias]
  implicit val SchemaJsonEncoder: JsonEncoder[Concept]         = DeriveJsonEncoder.gen[Concept]

  implicit val QualifiedNameEncoder: JsonEncoder[QualifiedName] =
    JsonEncoder.string.contramap {
      (qname: QualifiedName) => qname.toString
    }
}

trait DataZioJsonSupport extends DataZioJsonEncoders { self: SchemaZioJsonSupport => }

trait DataZioJsonEncoders extends ZioJsonBaseEncoders { self: SchemaZioJsonEncoders =>
  // implicit val DataJsonEncoder:JsonEncoder[Data] = DeriveJsonEncoder.gen[Data]
}

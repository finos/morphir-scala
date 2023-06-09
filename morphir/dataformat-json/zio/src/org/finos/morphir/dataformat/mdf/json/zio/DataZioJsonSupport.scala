package org.finos.morphir.dataformat
package mdf
package json.zio

import zio.json._
trait DataZioJsonSupport extends DataZioJsonEncoders { self: SchemaZioJsonSupport => }

trait DataZioJsonEncoders extends ZioJsonBaseEncoders { self: SchemaZioJsonEncoders =>
  // implicit val DataJsonEncoder:JsonEncoder[Data] = DeriveJsonEncoder.gen[Data]
}

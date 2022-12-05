package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ValueSpecificationDeriveGen {
  implicit def valueSpecificationDeriveGen[TA: DeriveGen]: DeriveGen[Value.Specification[TA]] =
    DeriveGen.instance(ValueSpecificationGen.valueSpecificationFromAttributes(DeriveGen[TA]))
}

object ValueSpecificationDeriveGen extends ValueSpecificationDeriveGen

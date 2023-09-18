package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Value.Value
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ValueDefinitionDeriveGen {
  implicit def valueDefinitionDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Definition[TA, VA]] =
    DeriveGen.instance(ValueDefinitionGen.valueDefinitionFromAttributes(DeriveGen[TA], DeriveGen[VA]))
}

object ValueDefinitionDeriveGen extends ValueDefinitionDeriveGen

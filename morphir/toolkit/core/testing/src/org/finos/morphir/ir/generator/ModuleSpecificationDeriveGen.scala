package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ModuleSpecificationDeriveGen {
  implicit def moduleSpecificationDeriveGen[TA: DeriveGen]: DeriveGen[Module.Specification[TA]] =
    DeriveGen.instance(ModuleSpecificationGen.moduleSpecificationFromAttributes(DeriveGen[TA]))
}

object ModuleSpecificationDeriveGen extends ModuleSpecificationDeriveGen

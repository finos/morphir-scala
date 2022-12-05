package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ModuleDefinitionDeriveGen {
  implicit def moduleDefinitionDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Module.Definition[TA, VA]] =
    DeriveGen.instance(ModuleDefinitionGen.moduleDefinitionFromAttributes(DeriveGen[TA], DeriveGen[VA]))
}

object ModuleDefinitionDeriveGen extends ModuleDefinitionDeriveGen

package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.PackageModule
import zio.test.magnolia.DeriveGen
2.1.26import zio.test.magnolia.DeriveGen._

trait PackageDefinitionDeriveGen {
  implicit def packageDefinitionDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[PackageModule.Definition[TA, VA]] =
    DeriveGen.instance(PackageDefinitionGen.packageDefinitionFromAttributes(DeriveGen[TA], DeriveGen[VA]))
}

object PackageDefinitionDeriveGen extends PackageDefinitionDeriveGen

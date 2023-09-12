package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait PackageSpecificationDeriveGen {
  implicit def packageSpecificationDeriveGen[TA: DeriveGen]: DeriveGen[PackageModule.Specification[TA]] =
    DeriveGen.instance(PackageSpecificationGen.packageSpecificationFromAttributes(DeriveGen[TA]))
}

object PackageSpecificationDeriveGen extends PackageSpecificationDeriveGen

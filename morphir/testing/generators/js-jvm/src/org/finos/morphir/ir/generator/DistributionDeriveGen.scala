package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.distribution.Distribution
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait DistributionDeriveGen {
  implicit val libraryDeriveGen: DeriveGen[Distribution.Library] =
    DeriveGen.instance(DistributionGen.libraryDistribution)

  implicit val distributionDeriveGen: DeriveGen[Distribution] =
    DeriveGen.instance(DistributionGen.distribution)
}

object DistributionDeriveGen extends DistributionDeriveGen

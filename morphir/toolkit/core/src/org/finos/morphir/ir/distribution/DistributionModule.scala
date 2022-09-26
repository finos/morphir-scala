package org.finos
package morphir
package ir
package distribution

trait DistributionModule {
  final type Distribution = morphir.ir.distribution.Distribution
  final val Distribution: morphir.ir.distribution.Distribution.type = morphir.ir.distribution.Distribution
}

object DistributionModule extends DistributionModule

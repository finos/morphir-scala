package org.finos
package morphir
package mir
package distribution

trait DistributionModule {
  final type Distribution = morphir.mir.distribution.Distribution
  final val Distribution: morphir.mir.distribution.Distribution.type = morphir.mir.distribution.Distribution
}

object DistributionModule extends DistributionModule

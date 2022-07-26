package morphir.ir.distribution

trait DistributionModule {
  final type Distribution = morphir.ir.distribution.Distribution
  final val Distribution: morphir.ir.distribution.Distribution.type = morphir.ir.distribution.Distribution
}

object DistributionModule extends DistributionModule

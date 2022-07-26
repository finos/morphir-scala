package morphir.ir.packages

trait PackageModule {

  final type Definition[+TA, +VA] = morphir.ir.packages.Definition[TA, VA]
  final val Definition: morphir.ir.packages.Definition.type = morphir.ir.packages.Definition

  final type PackageAndModulePath = morphir.ir.packages.PackageAndModulePath
  final val PackageAndModulePath: morphir.ir.packages.PackageAndModulePath.type =
    morphir.ir.packages.PackageAndModulePath

  final type PackageName = morphir.ir.packages.PackageName
  final val PackageName: morphir.ir.packages.PackageName.type = morphir.ir.packages.PackageName

  final type PackageSpecFor[A] = morphir.ir.packages.PackageSpecFor[A]
  final val PackageSpecFor: morphir.ir.packages.PackageSpecFor.type = morphir.ir.packages.PackageSpecFor

  final type Specification[+TA] = morphir.ir.packages.Specification[TA]
  final val Specification: morphir.ir.packages.Specification.type = morphir.ir.packages.Specification

  final type USpecification = morphir.ir.packages.Specification[Any]
  final val USpecification: morphir.ir.packages.Specification.type = morphir.ir.packages.Specification

  val emptySpecification: Specification[Nothing] = Specification.empty
}

object PackageModule extends PackageModule

package org.finos
package morphir
package mir
package packages

trait PackageModule {

  final type Definition[+TA, +VA] = morphir.mir.packages.Definition[TA, VA]
  final val Definition: morphir.mir.packages.Definition.type = morphir.mir.packages.Definition

  final type PackageAndModulePath = morphir.mir.packages.PackageAndModulePath
  final val PackageAndModulePath: morphir.mir.packages.PackageAndModulePath.type =
    morphir.mir.packages.PackageAndModulePath

  final type PackageName = morphir.mir.packages.PackageName
  final val PackageName: morphir.mir.packages.PackageName.type = morphir.mir.packages.PackageName

  final type PackageSpecFor[A] = morphir.mir.packages.PackageSpecFor[A]
  final val PackageSpecFor: morphir.mir.packages.PackageSpecFor.type = morphir.mir.packages.PackageSpecFor

  final type Specification[+TA] = morphir.mir.packages.Specification[TA]
  final val Specification: morphir.mir.packages.Specification.type = morphir.mir.packages.Specification

  final type USpecification = morphir.mir.packages.Specification[Any]
  final val USpecification: morphir.mir.packages.Specification.type = morphir.mir.packages.Specification

  val emptySpecification: Specification[Nothing] = Specification.empty
}

object PackageModule extends PackageModule

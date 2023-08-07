package org.finos
package morphir
package ir
package packages

trait PackageModule {

  final type Definition[+TA, +VA] = morphir.ir.packages.Definition[TA, VA]
  final val Definition: morphir.ir.packages.Definition.type = morphir.ir.packages.Definition

  final type Specification[+TA] = morphir.ir.packages.Specification[TA]
  final val Specification: morphir.ir.packages.Specification.type = morphir.ir.packages.Specification

  final type USpecification = morphir.ir.packages.Specification[scala.Unit]
  final val USpecification: morphir.ir.packages.Specification.type = morphir.ir.packages.Specification

  val emptySpecification: Specification[Nothing] = Specification.empty
}

object PackageModule extends PackageModule

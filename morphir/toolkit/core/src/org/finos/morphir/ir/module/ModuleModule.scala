package org.finos
package morphir
package ir
package module

trait ModuleModule {

  final type Definition[+TA, +VA] = morphir.ir.module.Definition[TA, VA]
  final val Definition: morphir.ir.module.Definition.type = morphir.ir.module.Definition

  final type ModuleName = morphir.ir.module.QualifiedModuleName
  final val ModuleName: morphir.ir.module.QualifiedModuleName.type = morphir.ir.module.QualifiedModuleName

  final type ModulePath = morphir.ir.module.ModulePath
  final val ModulePath: morphir.ir.module.ModulePath.type = morphir.ir.module.ModulePath

  final type QualifiedModuleName = morphir.ir.module.QModuleName
  final val QualifiedModuleName: morphir.ir.module.QModuleName.type =
    morphir.ir.module.QModuleName

  final type Specification[+TA] = morphir.ir.module.Specification[TA]
  final val Specification: morphir.ir.module.Specification.type = morphir.ir.module.Specification

  final type USpecification = morphir.ir.module.Specification[Any]
  final val USpecification: morphir.ir.module.Specification.type = morphir.ir.module.Specification

  final val emptyDefinition: Definition[Nothing, Nothing] = Definition.empty
  final val emptySpecification: Specification[Nothing]    = Specification.empty

  final def lookupValueDefinition[TA, VA](
      localName: Name,
      moduleDef: Definition[TA, VA]
  ): Option[Value.Definition[TA, VA]] =
    moduleDef.lookupValueDefinition(localName)
}

object ModuleModule extends ModuleModule

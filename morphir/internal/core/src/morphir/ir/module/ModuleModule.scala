package morphir.mir.module

import morphir.mir.{Name, Value}

trait ModuleModule {

  final type Definition[+TA, +VA] = morphir.mir.module.Definition[TA, VA]
  final val Definition: morphir.mir.module.Definition.type = morphir.mir.module.Definition

  final type ModuleName = morphir.mir.module.ModuleName
  final val ModuleName: morphir.mir.module.ModuleName.type = morphir.mir.module.ModuleName

  final type ModulePath = morphir.mir.module.ModulePath
  final val ModulePath: morphir.mir.module.ModulePath.type = morphir.mir.module.ModulePath

  final type QualifiedModuleName = morphir.mir.module.QualifiedModuleName
  final val QualifiedModuleName: morphir.mir.module.QualifiedModuleName.type =
    morphir.mir.module.QualifiedModuleName

  final type Specification[+TA] = morphir.mir.module.Specification[TA]
  final val Specification: morphir.mir.module.Specification.type = morphir.mir.module.Specification

  final type USpecification = morphir.mir.module.Specification[Any]
  final val USpecification: morphir.mir.module.Specification.type = morphir.mir.module.Specification

  final val emptyDefinition: Definition[Nothing, Nothing] = Definition.empty
  final val emptySpecification: Specification[Nothing]    = Specification.empty

  final def lookupValueDefinition[TA, VA](
      localName: Name,
      moduleDef: Definition[TA, VA]
  ): Option[Value.Definition[TA, VA]] =
    moduleDef.lookupValueDefinition(localName)
}

object ModuleModule extends ModuleModule

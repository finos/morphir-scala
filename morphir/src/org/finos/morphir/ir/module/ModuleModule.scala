package org.finos
package morphir
package ir
package module

import org.finos.morphir.naming.*

trait ModuleModule {

  final type Definition[+TA, +VA] = morphir.ir.module.Definition[TA, VA]
  final val Definition: morphir.ir.module.Definition.type = morphir.ir.module.Definition

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

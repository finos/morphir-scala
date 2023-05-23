package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Module.QualifiedModuleName
import zio.test.Gen

trait PackageDefinitionGen {
  final def packageDefinition[R, TA, VA](
      modulesGen: Gen[R, Map[QualifiedModuleName, AccessControlled[Module.Definition[TA, VA]]]]
  ): Gen[R, PackageModule.Definition[TA, VA]] =
    for {
      modules <- modulesGen
    } yield PackageModule.Definition(modules)

  final def packageDefinitionFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, PackageModule.Definition[TA, VA]] = packageDefinition(mapOfModulesGen)

  private final def mapOfModulesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Map[QualifiedModuleName, AccessControlled[Module.Definition[TA, VA]]]] =
    Gen.mapOfBounded(1, 2)(
      ModuleNameGen.moduleName,
      AccessControlledGen.accessControlledFromAttributes(
        ModuleDefinitionGen.moduleDefinitionFromAttributes(typeAttributes, valueAttributes)
      )
    )
}

object PackageDefinitionGen extends PackageDefinitionGen

package org.finos.morphir
package ir
package generator

import zio.test.Gen

trait PackageSpecificationGen {
  final def packageSpecification[R, TA](
      modulesGen: Gen[R, Map[Module.QualifiedModuleName, Module.Specification[TA]]]
  ): Gen[R, PackageModule.Specification[TA]] = for {
    modules <- modulesGen
  } yield PackageModule.Specification(modules)

  final def packageSpecificationFromAttributes[R, TA](implicit
      typeAttributes: Gen[R, TA]
  ): Gen[R, PackageModule.Specification[TA]] =
    packageSpecification(mapOfModulesGen)

  private final def mapOfModulesGen[R, TA](implicit
      typeAttributes: Gen[R, TA]
  ): Gen[R, Map[Module.QualifiedModuleName, Module.Specification[TA]]] =
    Gen.mapOfBounded(1, 2)(
      QualifiedModuleNameGen.qualifiedModuleName,
      ModuleSpecificationGen.moduleSpecificationFromAttributes(typeAttributes)
    )
}

object PackageSpecificationGen extends PackageSpecificationGen

package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import zio.test.Gen

trait DistributionGen {

  final def libraryDistribution(
      packageNameGen: Gen[Any, PackageName],
      dependenciesGen: Gen[Any, Map[PackageName, UPackageSpecification]],
      packageDefGen: Gen[Any, PackageDefinition.Typed]
  ): Gen[Any, Distribution.Library] = for {
    packageName  <- packageNameGen
    dependencies <- dependenciesGen
    packageDef   <- packageDefGen
  } yield Distribution.Library(packageName, dependencies, packageDef)

  final val libraryDistribution: Gen[Any, Distribution.Library] =
    libraryDistribution(
      PackageNameGen.packageName,
      mapOfDependenciesGen,
      PackageDefinitionGen.packageDefinitionFromAttributes(Gen.unit, TypeGen.typeGen(Gen.unit))
    )

  final def bundleDistribution(
      mapOfLibGen: Gen[Any, Map[PackageName, Distribution.Lib]]
  ): Gen[Any, Distribution.Bundle] = for {
    mapOfLib <- mapOfLibGen
  } yield Distribution.Bundle(mapOfLib)

  final val bundleDistribution: Gen[Any, Distribution.Bundle] =
    bundleDistribution(mapOfLibGen)

  final def distribution: Gen[Any, Distribution] = Gen.oneOf(libraryDistribution, bundleDistribution)

  private final def libDistribution(
      dependenciesGen: Gen[Any, Map[PackageName, UPackageSpecification]],
      packageDefGen: Gen[Any, PackageDefinition.Typed]
  ): Gen[Any, Distribution.Lib] = for {
    dependencies <- dependenciesGen
    packageDef   <- packageDefGen
  } yield Distribution.Lib(dependencies, packageDef)

  private final def libDistribution: Gen[Any, Distribution.Lib] =
    libDistribution(
      mapOfDependenciesGen,
      PackageDefinitionGen.packageDefinitionFromAttributes(Gen.unit, TypeGen.typeGen(Gen.unit))
    )

  private final def mapOfLibGen: Gen[Any, Map[PackageName, Distribution.Lib]] =
    Gen.mapOfBounded(2, 3)(
      PackageNameGen.packageName,
      libDistribution
    )

  private final def mapOfDependenciesGen: Gen[Any, Map[PackageName, PackageSpecification[Unit]]] =
    Gen.mapOfBounded(1, 2)(
      PackageNameGen.packageName,
      PackageSpecificationGen.packageSpecificationFromAttributes(Gen.unit)
    )
}

object DistributionGen extends DistributionGen

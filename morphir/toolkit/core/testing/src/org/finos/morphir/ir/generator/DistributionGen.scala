package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  PackageName,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import zio.test.Gen
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

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

  final def distribution: Gen[Any, Distribution] = Gen.oneOf(libraryDistribution)

  private final def mapOfDependenciesGen: Gen[Any, Map[PackageName, PackageSpecification[Unit]]] =
    Gen.mapOfBounded(1, 2)(
      PackageNameGen.packageName,
      PackageSpecificationGen.packageSpecificationFromAttributes(Gen.unit)
    )
}

object DistributionGen extends DistributionGen

package org.finos.morphir
package ir
package generator

trait MorphirIRGen
    extends AccessControlledGen
    with ConstructorsGen
    with DistributionGen
    with DocumentedGen
    with FieldGen
    with FQNameGen
    with LiteralGen
    with ModuleDefinitionGen
    with QualifiedModuleNameGen
    with ModulePathGen
    with ModuleSpecificationGen
    with MorphirIRFileGen
    with NameGen
    with PackageDefinitionGen
    with PackageNameGen
    with PackageSpecificationGen
    with PathGen
    with PatternGen
    with QNameGen
    with TypeDefinitionGen
    with TypeGen
    with TypeSpecificationGen
    with ValueDefinitionGen
    with ValueGen
    with ValueSpecificationGen

object MorphirIRGen extends MorphirIRGen

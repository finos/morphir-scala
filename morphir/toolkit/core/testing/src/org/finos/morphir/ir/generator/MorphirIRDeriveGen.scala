package org.finos.morphir
package ir
package generator

trait MorphirIRDeriveGen
    extends AccessControlledDeriveGen
    with ConstructorsDeriveGen
    with DistributionDeriveGen
    with DocumentedDeriveGen
    with FieldDeriveGen
    with FQNameDeriveGen
    with LiteralDeriveGen
    with ModuleDefinitionDeriveGen
    with QualifiedModuleNameDeriveGen
    with ModuleNameDeriveGen
    with ModuleSpecificationDeriveGen
    with MorphirIRFileDeriveGen
    with NameDeriveGen
    with PackageDefinitionDeriveGen
    with PackageNameDeriveGen
    with PackageSpecificationDeriveGen
    with PathDeriveGen
    with PatternDeriveGen
    with QNameDeriveGen
    with TypeDefinitionDeriveGen
    with TypeDeriveGen
    with TypeSpecificationDeriveGen
    with ValueDefinitionDeriveGen
    with ValueDeriveGen
    with ValueSpecificationDeriveGen

object MorphirIRDeriveGen extends MorphirIRDeriveGen

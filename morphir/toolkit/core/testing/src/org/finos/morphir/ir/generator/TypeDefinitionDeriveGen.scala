package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Definition
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait TypeDefinitionDeriveGen {
  implicit def typeAliasDefinitionDeriveGen[A: DeriveGen]: DeriveGen[Definition.TypeAlias[A]] =
    DeriveGen.instance(TypeDefinitionGen.typeAliasDefinitionFromAttributes(DeriveGen[A]))

  implicit def customTypeDefinitionDeriveGen[A: DeriveGen]: DeriveGen[Definition.CustomType[A]] =
    DeriveGen.instance(TypeDefinitionGen.customTypeDefinitionFromAttributes(DeriveGen[A]))

  implicit def definitionDeriveGen[A: DeriveGen]: DeriveGen[Definition[A]] =
    DeriveGen.instance(TypeDefinitionGen.typeDefinition(DeriveGen[A]))
}

object TypeDefinitionDeriveGen extends TypeDefinitionDeriveGen

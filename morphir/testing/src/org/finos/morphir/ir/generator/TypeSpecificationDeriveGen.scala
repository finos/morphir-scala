package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Specification
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait TypeSpecificationDeriveGen {
  implicit def customTypeSpecificationDeriveGen[A: DeriveGen]: DeriveGen[Specification.CustomTypeSpecification[A]] =
    DeriveGen.instance(TypeSpecificationGen.customTypeSpecificationFromAttributes(DeriveGen[A]))

  implicit def opaqueTypeSpecificationDeriveGen: DeriveGen[Specification.OpaqueTypeSpecification] =
    DeriveGen.instance(TypeSpecificationGen.opaqueTypeSpecificationFromAttributes)

  implicit def typeAliasSpecificationDeriveGen[A: DeriveGen]: DeriveGen[Specification.TypeAliasSpecification[A]] =
    DeriveGen.instance(TypeSpecificationGen.typeAliasSpecificationFromAttributes(DeriveGen[A]))

  implicit def typeSpecificationDeriveGen[A: DeriveGen]: DeriveGen[Specification[A]] =
    DeriveGen.instance(TypeSpecificationGen.typeSpecification(DeriveGen[A]))
}

object TypeSpecificationDeriveGen extends TypeSpecificationDeriveGen

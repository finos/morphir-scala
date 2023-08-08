package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.Type.Constructors
import org.finos.morphir.ir.Type.Specification
import org.finos.morphir.ir.Type.Type
import zio._
import zio.test.Gen

trait TypeSpecificationGen {

  final def customTypeSpecification[R, A](
      typeParamsGen: Gen[R, Chunk[Name]],
      ctorsGen: Gen[R, Constructors[A]]
  ): Gen[R, Specification.CustomTypeSpecification[A]] = for {
    typeParams <- typeParamsGen
    ctors      <- ctorsGen
  } yield Specification.CustomTypeSpecification(typeParams, ctors)

  final def customTypeSpecificationFromAttributes[R, A](implicit
      attributes: Gen[R, A]
  ): Gen[R, Specification.CustomTypeSpecification[A]] =
    customTypeSpecification(typeParamsGen, ctorsGen)

  final def typeAliasSpecification[R, A](
      typeParamsGen: Gen[R, Chunk[Name]],
      exprGen: Gen[R, Type[A]]
  ): Gen[R, Specification.TypeAliasSpecification[A]] = for {
    typeParams <- typeParamsGen
    expr       <- exprGen
  } yield Specification.TypeAliasSpecification(typeParams, expr)

  final def typeAliasSpecificationFromAttributes[R, A](implicit
      attributes: Gen[R, A]
  ): Gen[R, Specification.TypeAliasSpecification[A]] =
    typeAliasSpecification(typeParamsGen, TypeGen.typeGen(attributes))

  final def opaqueTypeSpecification[R, A](
      typeParamsGen: Gen[R, Chunk[Name]]
  ): Gen[R, Specification.OpaqueTypeSpecification] =
    for {
      typeParams <- typeParamsGen
    } yield Specification.OpaqueTypeSpecification(typeParams)

  final def opaqueTypeSpecificationFromAttributes[R, A]: Gen[R, Specification.OpaqueTypeSpecification] =
    opaqueTypeSpecification(typeParamsGen)

  final def typeSpecification[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Specification[A]] =
    Gen.oneOf(
      customTypeSpecificationFromAttributes,
      typeAliasSpecificationFromAttributes,
      opaqueTypeSpecificationFromAttributes
    )

  private final def typeParamsGen[R, A]: Gen[R, Chunk[Name]] = Gen.chunkOfBounded[R, Name](1, 3)(NameGen.name)

  private final def ctorsGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Constructors[A]] =
    ConstructorsGen.constructorsFromAttributes(attributesGen)
}

object TypeSpecificationGen extends TypeSpecificationGen

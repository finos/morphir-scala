package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Constructors
import org.finos.morphir.ir.Type.Definition
import org.finos.morphir.ir.Type.Type
import zio._
import zio.test.Gen

trait TypeDefinitionGen {

  final def customTypeDefinition[R, A](
      typeParamsGen: Gen[R, Chunk[Name]],
      ctorsGen: Gen[R, AccessControlled[Constructors[A]]]
  ): Gen[R, Definition.CustomType[A]] = for {
    typeParams <- typeParamsGen
    ctors      <- ctorsGen
  } yield Definition.CustomType(typeParams, ctors)

  final def customTypeDefinitionFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Definition.CustomType[A]] =
    customTypeDefinition(typeParamsGen, ctorsGen)

  final def typeAliasDefinition[R, A](
      typeParamsGen: Gen[R, Chunk[Name]],
      typeExpGen: Gen[R, Type[A]]
  ): Gen[R, Definition.TypeAlias[A]] = for {
    typeParams <- typeParamsGen
    typeExp    <- typeExpGen
  } yield Definition.TypeAlias(typeParams, typeExp)

  final def typeAliasDefinitionFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Definition.TypeAlias[A]] =
    typeAliasDefinition(typeParamsGen, TypeGen.typeGen(attributes))

  final def typeDefinition[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Definition[A]] =
    Gen.oneOf(
      typeAliasDefinitionFromAttributes,
      customTypeDefinitionFromAttributes
    )

  private final def typeParamsGen[R, A]: Gen[R, Chunk[Name]] = Gen.chunkOfBounded[R, Name](1, 2)(NameGen.name)

  private final def ctorsGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, AccessControlled[Constructors[A]]] =
    AccessControlledGen.accessControlled(
      AccessControlledGen.access,
      ConstructorsGen.constructorsFromAttributes
    )
}

object TypeDefinitionGen extends TypeDefinitionGen

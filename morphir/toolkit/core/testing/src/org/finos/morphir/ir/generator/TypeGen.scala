package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Type
import zio._
import zio.test.Gen

trait TypeGen {
  final def extensibleRecord[R, A](
      attributesGen: Gen[R, A],
      nameGen: Gen[R, Name],
      fieldsGen: Gen[R, Chunk[Field[Type[A]]]]
  ): Gen[R, Type.ExtensibleRecord[A]] =
    for {
      attributes <- attributesGen
      name       <- nameGen
      fields     <- fieldsGen
    } yield Type.ExtensibleRecord(attributes, name, fields)

  final def extensibleRecordFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.ExtensibleRecord[A]] =
    extensibleRecord(attributes, NameGen.name, chunkOfFieldsTypesGen)

  final def function[R, A](attributesGen: Gen[R, A], typeGen: Gen[R, Type[A]]): Gen[R, Type.Function[A]] = for {
    attributes   <- attributesGen
    argumentType <- typeGen
    returnType   <- typeGen
  } yield Type.Function(attributes, argumentType, returnType)

  final def functionFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.Function[A]] =
    function(attributes, typeGen)

  final def recordType[R, A](
      attributesGen: Gen[R, A],
      fieldsGen: Gen[R, Chunk[Field[Type[A]]]]
  ): Gen[R, Type.Record[A]] =
    for {
      attributes <- attributesGen
      fields     <- fieldsGen
    } yield Type.Record(attributes, fields)

  final def recordTypeFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.Record[A]] =
    recordType(attributes, chunkOfFieldsTypesGen)

  final def referenceType[R, A](
      attributesGen: Gen[R, A],
      typeNameGen: Gen[R, FQName],
      typeParamsGen: Gen[R, Chunk[Type[A]]]
  ): Gen[R, Type.Reference[A]] = for {
    attributes <- attributesGen
    typeName   <- typeNameGen
    typeParams <- typeParamsGen
  } yield Type.Reference(attributes, typeName, typeParams)

  final def referenceTypeFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.Reference[A]] =
    referenceType(attributes, FQNameGen.fqName, chunkOfTypesGen)

  final def tupleType[R, A](attributesGen: Gen[R, A], elementsGen: Gen[R, Chunk[Type[A]]]): Gen[R, Type.Tuple[A]] =
    for {
      attributes <- attributesGen
      elements   <- elementsGen
    } yield Type.Tuple(attributes, elements)

  final def tupleTypeFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.Tuple[A]] =
    tupleType(attributes, chunkOfTypesGen)

  final def unitType[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Type.Unit[A]] = for {
    attributes <- attributesGen
  } yield Type.Unit(attributes)

  final def variableType[R, A](attributesGen: Gen[R, A], nameGen: Gen[R, Name]): Gen[R, Type.Variable[A]] = for {
    attributes <- attributesGen
    name       <- nameGen
  } yield Type.Variable(attributes, name)

  final def variableTypeFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Type.Variable[A]] =
    variableType(attributes, NameGen.name)

  final def typeGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Type[A]] =
    Gen.suspend(
      Gen.oneOf(
        typeGenRecursive, // this lowers the probability of recursive cases
        unitType,
        variableTypeFromAttributes
      )
    )

  private final def typeGenRecursive[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Type[A]] =
    Gen.suspend(
      Gen.oneOf(
        extensibleRecordFromAttributes,
        functionFromAttributes,
        recordTypeFromAttributes,
        referenceTypeFromAttributes,
        tupleTypeFromAttributes
      )
    )

  private final def chunkOfTypesGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Chunk[Type[A]]] =
    Gen.chunkOfBounded[R, Type[A]](1, 2)(typeGen)

  private final def chunkOfFieldsTypesGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Chunk[Field[Type[A]]]] =
    Gen.chunkOfBounded[R, Field[Type[A]]](1, 2)(FieldGen.fieldFromAttributes(typeGen))
}

object TypeGen extends TypeGen

package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Value.{Definition, Value}
import zio._
import zio.test.Gen

trait ValueDefinitionGen {
  final def valueDefinition[R, TA, VA](
      inputTypesGen: Gen[R, Chunk[(Name, VA, Type[TA])]],
      outputTypeGen: Gen[R, Type[TA]],
      bodyGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.Definition[TA, VA]] =
    for {
      inputTypes <- inputTypesGen
      outputType <- outputTypeGen
      body       <- bodyGen
    } yield Value.Definition(inputTypes, outputType, body)

  final def valueDefinitionFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Definition[TA, VA]] =
    valueDefinition(
      chunkOfNameToValueAttributesToTypeGen(typeAttributes, valueAttributes),
      TypeGen.typeGen(typeAttributes),
      ValueGen.value(typeAttributes, valueAttributes)
    )

  private final def chunkOfNameToValueAttributesToTypeGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Chunk[(Name, VA, Type[TA])]] =
    Gen.chunkOfBounded(1, 2)(NameGen.name <*> valueAttributes <*> TypeGen.typeGen(typeAttributes))
}

object ValueDefinitionGen extends ValueDefinitionGen

package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.Type.Type
import zio._
import zio.test.Gen

trait ValueSpecificationGen {

  final def valueSpecification[R, TA](
      inputsGen: Gen[R, Chunk[(Name, Type[TA])]],
      outputGen: Gen[R, Type[TA]]
  ): Gen[R, Value.Specification[TA]] = for {
    inputs <- inputsGen
    output <- outputGen
  } yield Value.Specification(inputs, output)

  final def valueSpecificationFromAttributes[R, TA](implicit
      typeAttributesGen: Gen[R, TA]
  ): Gen[R, Value.Specification[TA]] =
    valueSpecification(chunkOfFieldsTypesGen, TypeGen.typeGen)

  private final def chunkOfFieldsTypesGen[R, TA](implicit
      typeAttributesGen: Gen[R, TA]
  ): Gen[R, Chunk[(Name, Type[TA])]] =
    Gen.chunkOfBounded(1, 2)(NameGen.name <*> TypeGen.typeGen)
}

object ValueSpecificationGen extends ValueSpecificationGen

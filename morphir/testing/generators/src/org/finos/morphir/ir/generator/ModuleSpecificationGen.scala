package org.finos.morphir.ir.generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.Documented
import org.finos.morphir.ir.Type.{Specification => TypeSpec}
import org.finos.morphir.ir.Value.{Specification => ValueSpec}
import org.finos.morphir.ir.module.{Specification => ModSpec}
import zio.test.Gen

trait ModuleSpecificationGen {
  final def moduleSpecification[R, TA](
      typesGen: Gen[R, Map[Name, Documented[TypeSpec[TA]]]],
      valuesGen: Gen[R, Map[Name, Documented[ValueSpec[TA]]]]
  ): Gen[R, ModSpec[TA]] = for {
    types  <- typesGen
    values <- valuesGen
  } yield ModSpec(types, values)

  final def moduleSpecificationFromAttributes[R, TA](implicit typeAttributes: Gen[R, TA]): Gen[R, ModSpec[TA]] =
    moduleSpecification(mapOfNameToDocumentedTypeSpecGen, mapOfNameToDocumentedValueSpecGen)

  private final def mapOfNameToDocumentedTypeSpecGen[R, TA](implicit
      typeAttributes: Gen[R, TA]
  ): Gen[R, Map[Name, Documented[TypeSpec[TA]]]] =
    Gen.mapOfBounded(1, 2)(
      NameGen.name,
      DocumentedGen.documentedFromAttributes(TypeSpecificationGen.typeSpecification(typeAttributes))
    )

  private final def mapOfNameToDocumentedValueSpecGen[R, TA](implicit
      typeAttributes: Gen[R, TA]
  ): Gen[R, Map[Name, Documented[ValueSpec[TA]]]] =
    Gen.mapOfBounded(1, 2)(
      NameGen.name,
      DocumentedGen.documentedFromAttributes(ValueSpecificationGen.valueSpecificationFromAttributes(typeAttributes))
    )
}

object ModuleSpecificationGen extends ModuleSpecificationGen

package org.finos.morphir
package ir
package generator

import zio.test.Gen

trait ModuleDefinitionGen {
  final def moduleDefinition[R, TA, VA](
      typesGen: Gen[R, Map[Name, AccessControlled[Documented[Type.Definition[TA]]]]],
      valuesGen: Gen[R, Map[Name, AccessControlled[Documented[Value.Definition[TA, VA]]]]]
  ): Gen[R, Module.Definition[TA, VA]] =
    for {
      types  <- typesGen
      values <- valuesGen
    } yield Module.Definition(types, values)

  final def moduleDefinitionFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Module.Definition[TA, VA]] =
    moduleDefinition(
      mapOfTypesGen(typeAttributes),
      mapOfValuesGen(typeAttributes, valueAttributes)
    )

  private final def mapOfTypesGen[R, TA](implicit
      typeAttributes: Gen[R, TA]
  ): Gen[R, Map[Name, AccessControlled[Documented[Type.Definition[TA]]]]] =
    Gen.mapOfBounded(1, 2)(
      NameGen.name,
      AccessControlledGen.accessControlledFromAttributes(
        DocumentedGen.documentedFromAttributes(
          TypeDefinitionGen.typeDefinition(typeAttributes)
        )
      )
    )

  private final def mapOfValuesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Map[Name, AccessControlled[Documented[Value.Definition[TA, VA]]]]] =
    Gen.mapOfBounded(1, 2)(
      NameGen.name,
      AccessControlledGen.accessControlledFromAttributes(
        DocumentedGen.documentedFromAttributes(
          ValueDefinitionGen.valueDefinitionFromAttributes(typeAttributes, valueAttributes)
        )
      )
    )
}

object ModuleDefinitionGen extends ModuleDefinitionGen

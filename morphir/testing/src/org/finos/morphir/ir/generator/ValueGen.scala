package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Value.{Definition, Pattern, Value}
import zio._
import zio.test.Gen

trait ValueGen {
  final def apply[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      functionGen: Gen[R, Value[TA, VA]],
      argumentGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.Apply[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      function        <- functionGen
      argument        <- argumentGen
    } yield Value.Apply(valueAttributes, function, argument)

  final def applyFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Apply[TA, VA]] =
    apply(valueAttributes, value, value)

  final def constructor[R, VA](valueAttributesGen: Gen[R, VA], fullyQualifiedNameGen: Gen[R, FQName]) =
    for {
      valueAttributes <- valueAttributesGen
      fqn             <- fullyQualifiedNameGen
    } yield Value.Constructor(valueAttributes, fqn)

  final def constructorFromAttributes[R, VA](implicit valueAttributes: Gen[R, VA]): Gen[R, Value.Constructor[VA]] =
    constructor(valueAttributes, FQNameGen.fqName)

  final def destructure[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      patternGen: Gen[R, Pattern[VA]],
      valueToDestructGen: Gen[R, Value[TA, VA]],
      inValueGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.Destructure[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      pattern         <- patternGen
      valueToDestruct <- valueToDestructGen
      inValue         <- inValueGen
    } yield Value.Destructure(valueAttributes, pattern, valueToDestruct, inValue)

  final def destructureFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Destructure[TA, VA]] =
    destructure(valueAttributes, PatternGen.pattern, value, value)

  final def field[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      subjectValueGen: Gen[R, Value[TA, VA]],
      fieldNameGen: Gen[R, Name]
  ): Gen[R, Value.Field[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      subjectValue    <- subjectValueGen
      fieldName       <- fieldNameGen
    } yield Value.Field(valueAttributes, subjectValue, fieldName)

  final def fieldFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Field[TA, VA]] =
    field(valueAttributes, value, NameGen.name)

  final def fieldFunction[R, VA](
      valueAttributesGen: Gen[R, VA],
      nameGen: Gen[R, Name]
  ): Gen[R, Value.FieldFunction[VA]] =
    for {
      valueAttributes <- valueAttributesGen
      name            <- nameGen
    } yield Value.FieldFunction(valueAttributes, name)

  final def fieldFunctionFromAttributes[R, VA](implicit valueAttributes: Gen[R, VA]): Gen[R, Value.FieldFunction[VA]] =
    fieldFunction(valueAttributes, NameGen.name)

  final def ifThenElse[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      conditionGen: Gen[R, Value[TA, VA]],
      thenBranchGen: Gen[R, Value[TA, VA]],
      elseBranchGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.IfThenElse[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      condition       <- conditionGen
      thenBranch      <- thenBranchGen
      elseBranch      <- elseBranchGen
    } yield Value.IfThenElse(valueAttributes, condition, thenBranch, elseBranch)

  final def ifThenElseFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.IfThenElse[TA, VA]] =
    ifThenElse(valueAttributes, value, value, value)

  final def lambda[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      argumentPatternGen: Gen[R, Pattern[VA]],
      bodyGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.Lambda[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      argumentPattern <- argumentPatternGen
      body            <- bodyGen
    } yield Value.Lambda(valueAttributes, argumentPattern, body)

  final def lambdaFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Lambda[TA, VA]] =
    lambda(valueAttributes, PatternGen.pattern, value)

  final def letDefinition[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      valueNameGen: Gen[R, Name],
      valueDefinitionGen: Gen[R, Definition[TA, VA]],
      inValueGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.LetDefinition[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      valueName       <- valueNameGen
      valueDefinition <- valueDefinitionGen
      inValue         <- inValueGen
    } yield Value.LetDefinition(valueAttributes, valueName, valueDefinition, inValue)

  final def letDefinitionFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.LetDefinition[TA, VA]] =
    letDefinition(valueAttributes, NameGen.name, ValueDefinitionGen.valueDefinitionFromAttributes, value)

  final def letRecursion[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      valueDefinitionsGen: Gen[R, Map[Name, Definition[TA, VA]]],
      inValueGen: Gen[R, Value[TA, VA]]
  ): Gen[R, Value.LetRecursion[TA, VA]] =
    for {
      valueAttributes  <- valueAttributesGen
      valueDefinitions <- valueDefinitionsGen
      inValue          <- inValueGen
    } yield Value.LetRecursion(valueAttributes, valueDefinitions, inValue)

  final def letRecursionFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.LetRecursion[TA, VA]] =
    letRecursion(valueAttributes, mapOfNameToValueDefinitionsGen, value)

  final def list[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      elementsGen: Gen[R, Chunk[Value[TA, VA]]]
  ): Gen[R, Value.List[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      elements        <- elementsGen
    } yield Value.List(valueAttributes, elements)

  final def listFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.List[TA, VA]] =
    list(valueAttributes, chunkOfValuesGen)

  final def literal[R, VA](
      valueAttributesGen: Gen[R, VA],
      literalGen: Gen[R, Literal]
  ): Gen[R, Value.Literal[VA]] =
    for {
      valueAttributes <- valueAttributesGen
      literal         <- literalGen
    } yield Value.Literal(valueAttributes, literal)

  final def literalFromAttributes[R, VA](implicit valueAttributes: Gen[R, VA]): Gen[R, Value.Literal[VA]] =
    literal(valueAttributes, LiteralGen.literal)

  final def patternMatch[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      branchOutOnGen: Gen[R, Value[TA, VA]],
      casesGen: Gen[R, Chunk[(Pattern[VA], Value[TA, VA])]]
  ): Gen[R, Value.PatternMatch[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      branchOutOn     <- branchOutOnGen
      cases           <- casesGen
    } yield Value.PatternMatch(valueAttributes, branchOutOn, cases)

  final def patternMatchFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.PatternMatch[TA, VA]] =
    patternMatch(valueAttributes, value, chunkOfPatternToValuesGen)

  final def recordValue[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      fieldsGen: Gen[R, Chunk[(Name, Value[TA, VA])]]
  ): Gen[R, Value.Record[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      fields          <- fieldsGen
    } yield Value.Record(valueAttributes, fields)

  final def recordValueFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Record[TA, VA]] =
    recordValue(valueAttributes, chunkOfNameToValuesGen)

  final def referenceValue[R, VA](valueAttributesGen: Gen[R, VA], fullyQualifiedNameGen: Gen[R, FQName]) =
    for {
      valueAttributes <- valueAttributesGen
      fqn             <- fullyQualifiedNameGen
    } yield Value.Reference(valueAttributes, fqn)

  final def referenceValueFromAttributes[R, VA](implicit valueAttributes: Gen[R, VA]): Gen[R, Value.Reference[VA]] =
    referenceValue(valueAttributes, FQNameGen.fqName)

  final def tupleValue[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      elementsGen: Gen[R, Chunk[Value[TA, VA]]]
  ): Gen[R, Value.Tuple[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      elements        <- elementsGen
    } yield Value.Tuple(valueAttributes, elements)

  final def tupleValueFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.Tuple[TA, VA]] =
    tupleValue(valueAttributes, chunkOfValuesGen)

  final def unitValue[R, VA](implicit valueAttributesGen: Gen[R, VA]): Gen[R, Value.Unit[VA]] =
    for {
      valueAttributes <- valueAttributesGen
    } yield Value.Unit(valueAttributes)

  final def updateRecord[R, TA, VA](
      valueAttributesGen: Gen[R, VA],
      valueToUpdateGen: Gen[R, Value[TA, VA]],
      fieldsToUpdateGen: Gen[R, Map[Name, Value[TA, VA]]]
  ): Gen[R, Value.UpdateRecord[TA, VA]] =
    for {
      valueAttributes <- valueAttributesGen
      valueToUpdate   <- valueToUpdateGen
      fieldsToUpdate  <- fieldsToUpdateGen
    } yield Value.UpdateRecord(valueAttributes, valueToUpdate, fieldsToUpdate)

  final def updateRecordFromAttributes[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value.UpdateRecord[TA, VA]] =
    updateRecord(valueAttributes, value, mapOfNameToValuesGen)

  final def variableValue[R, VA](
      valueAttributesGen: Gen[R, VA],
      nameGen: Gen[R, Name]
  ): Gen[R, Value.Variable[VA]] =
    for {
      valueAttributes <- valueAttributesGen
      name            <- nameGen
    } yield Value.Variable(valueAttributes, name)

  final def variableValueFromAttributes[R, VA](implicit valueAttributes: Gen[R, VA]): Gen[R, Value.Variable[VA]] =
    variableValue(valueAttributes, NameGen.name)

  final def value[R, TA, VA](implicit typeAttributes: Gen[R, TA], valueAttributes: Gen[R, VA]): Gen[R, Value[TA, VA]] =
    Gen.suspend(
      Gen.oneOf(
        valueRecursiveCases, // this lowers the probability of recursive cases
        constructorFromAttributes,
        fieldFunctionFromAttributes,
        literalFromAttributes,
        referenceValueFromAttributes,
        unitValue,
        variableValueFromAttributes
      )
    )

  private final def valueRecursiveCases[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Value[TA, VA]] =
    Gen.suspend(
      Gen.oneOf(
        applyFromAttributes,
        destructureFromAttributes,
        fieldFromAttributes,
        ifThenElseFromAttributes,
        lambdaFromAttributes,
        letDefinitionFromAttributes,
        letRecursionFromAttributes,
        listFromAttributes,
        patternMatchFromAttributes,
        recordValueFromAttributes,
        tupleValueFromAttributes,
        updateRecordFromAttributes
      )
    )

  private final def chunkOfValuesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Chunk[Value[TA, VA]]] =
    Gen.chunkOfBounded[R, Value[TA, VA]](1, 2)(value)

  private final def mapOfNameToValueDefinitionsGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Map[Name, Value.Definition[TA, VA]]] =
    Gen.mapOfBounded(1, 2)(
      NameGen.name,
      ValueDefinitionGen.valueDefinitionFromAttributes(typeAttributes, valueAttributes)
    )

  private final def mapOfNameToValuesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Map[Name, Value[TA, VA]]] =
    Gen.mapOfBounded(1, 2)(NameGen.name, value(typeAttributes, valueAttributes))

  private final def chunkOfNameToValuesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Chunk[(Name, Value[TA, VA])]] =
    Gen.chunkOfBounded(1, 2)(NameGen.name <*> value(typeAttributes, valueAttributes))

  private final def chunkOfPatternToValuesGen[R, TA, VA](implicit
      typeAttributes: Gen[R, TA],
      valueAttributes: Gen[R, VA]
  ): Gen[R, Chunk[(Pattern[VA], Value[TA, VA])]] =
    Gen.chunkOfBounded(1, 2)(PatternGen.pattern(valueAttributes) <*> value(typeAttributes, valueAttributes))
}

object ValueGen extends ValueGen

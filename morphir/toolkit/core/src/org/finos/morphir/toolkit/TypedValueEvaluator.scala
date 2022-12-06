package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO}
import org.finos.morphir.ir.{FQName, Name, Type}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.Pattern
import zio._
import org.finos.morphir.ir.Literal.Literal._
import EvaluationContext.{Variables, VariableRef}

class TypedValueEvaluator extends Evaluator[scala.Unit, UType] { self =>

  def applyCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      function: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      argument: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def constructorCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      name: FQName
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def destructureCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      pattern: Pattern[UType],
      valueToDestruct: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      inValue: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def fieldCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      subjectValue: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      fieldName: Name
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def fieldFunctionCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      fieldName: Name
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def ifThenElseCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      condition: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      thenBranch: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      elseBranch: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def lambdaCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      argumentPattern: Pattern[UType],
      body: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def letDefinitionCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      valueName: Name,
      valueDefinition: (
          Chunk[(Name, UType, Type.Type[Unit])],
          Type.Type[Unit],
          ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
      ),
      inValue: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = for {
    _ <- zio.Console.printLine(s"$value").orDie
    (params, _, body) = valueDefinition
    evaluatedBody <- body
    variable = (valueName, VariableRef.Evaluated(evaluatedBody, attributes))
    locals0      <- EvaluationContext.Typed.localVaribles
    _           <- Console.printLine(s"[Initial]Variables: $locals0").orDie
    _           <- EvaluationContext.Typed.pushFrame(Variables.Typed(variable))
    res         <- inValue
    locals      <- EvaluationContext.Typed.localVaribles
    _           <- Console.printLine(s"[Before pop]Variables: $locals").orDie
    _           <- EvaluationContext.Typed.popFrame
    localsAfter <- EvaluationContext.Typed.localVaribles
    _           <- Console.printLine(s"[After pop]Variables: $localsAfter").orDie
  } yield res

  /*
      let a = 1 in (let b = a in b

      //TODO: Push and pop "frames" on stack
      Let x = 3 in (let x = 2 in x) + x
   */

  /*
      let
        a = 5
      in let
        b = a
      in c = 2 * b
   */

  def letRecursionCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      valueDefinitions: Map[
        Name,
        (
            Chunk[(Name, UType, Type.Type[Unit])],
            Type.Type[Unit],
            ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
        )
      ],
      inValue: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def listCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      elements: Chunk[ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ZIO.collectAll(elements).map(_.toList)

  def literalCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      literal: Lit
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ZIO.succeed {
    // TODO: Look at the type annotation and convert to the correct type
    literal match {
      case StringLiteral(value)      => value
      case FloatLiteral(value)       => value
      case CharLiteral(value)        => value
      case BoolLiteral(value)        => value
      case WholeNumberLiteral(value) => value
      case DecimalLiteral(value)     => value
    }
  }

  def patternMatchCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      branchOutOn: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      cases: Chunk[(Pattern[UType], ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any])]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def recordCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      fields: Chunk[(Name, ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any])]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def referenceCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      name: FQName
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def tupleCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      elements: Chunk[ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ZIO.collectAll(elements).flatMap { items =>
    items match {
      case Chunk(a, b) => ZIO.succeed((a, b))
      case _           => ZIO.fail(EvaluationError.UnsupportedTupleArity(value, elements.size))
    }
  }

  def unitCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ZIO.succeed(())

  def updateRecordCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      valueToUpdate: ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any],
      fieldsToUpdate: Map[Name, ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any]]
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] = ???

  def variableCase(
      context: scala.Unit,
      value: Value[Unit, UType],
      attributes: UType,
      name: Name
  ): ZIO[ZEvaluationContext[scala.Unit, UType], EvaluationError, Any] =
    for {
      ctx           <- ZIO.service[ZEvaluationContext[scala.Unit, UType]]
      maybeVariable <- ctx.get.map(_.variables.get(name))
      variable      <- ZIO.fromOption(maybeVariable).mapError(_ => EvaluationError.VariableNotFound(name))
      resolved      <- variable.resolve(self)
    } yield resolved

}

object TypedValueEvaluator {
  def apply: TypedValueEvaluator = new TypedValueEvaluator
}

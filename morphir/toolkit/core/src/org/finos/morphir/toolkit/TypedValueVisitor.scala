package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO}
import org.finos.morphir.ir.{FQName, Name, Type}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.Pattern
import zio.Chunk
import org.finos.morphir.ir.Literal.Literal.StringLiteral
import org.finos.morphir.ir.Literal.Literal.FloatLiteral
import org.finos.morphir.ir.Literal.Literal.CharLiteral
import org.finos.morphir.ir.Literal.Literal.BoolLiteral
import org.finos.morphir.ir.Literal.Literal.WholeNumberLiteral
import org.finos.morphir.ir.Literal.Literal.DecimalLiteral

object TypedValueVisitor {
  val Default: ValueVisitor[Evaluator.TypedEvaluationContext, scala.Unit, UType] = new Default
  class Default extends ValueVisitor[Evaluator.TypedEvaluationContext, scala.Unit, UType] {

    def applyCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        function: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        argument: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def constructorCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        name: FQName
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def destructureCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        pattern: Pattern[UType],
        valueToDestruct: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        inValue: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def fieldCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        subjectValue: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        fieldName: Name
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def fieldFunctionCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        fieldName: Name
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def ifThenElseCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        condition: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        thenBranch: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        elseBranch: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def lambdaCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        argumentPattern: Pattern[UType],
        body: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def letDefinitionCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        valueName: Name,
        valueDefinition: (
            Chunk[(Name, UType, Type.Type[Unit])],
            Type.Type[Unit],
            ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
        ),
        inValue: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def letRecursionCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        valueDefinitions: Map[
          Name,
          (
              Chunk[(Name, UType, Type.Type[Unit])],
              Type.Type[Unit],
              ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
          )
        ],
        inValue: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def listCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        elements: Chunk[ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ZIO.collectAll(elements).map(_.toList)

    def literalCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        literal: Lit
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ZIO.succeed {
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
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        branchOutOn: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        cases: Chunk[(Pattern[UType], ZIO[Evaluator.TypedEvaluationContext, Throwable, Any])]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def recordCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        fields: Chunk[(Name, ZIO[Evaluator.TypedEvaluationContext, Throwable, Any])]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def referenceCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        name: FQName
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def tupleCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        elements: Chunk[ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def unitCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ZIO.succeed(())

    def updateRecordCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        valueToUpdate: ZIO[Evaluator.TypedEvaluationContext, Throwable, Any],
        fieldsToUpdate: Map[Name, ZIO[Evaluator.TypedEvaluationContext, Throwable, Any]]
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

    def variableCase(
        context: Evaluator.TypedEvaluationContext,
        value: Value[Unit, UType],
        attributes: UType,
        name: Name
    ): ZIO[Evaluator.TypedEvaluationContext, Throwable, Any] = ???

  }
}

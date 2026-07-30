package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk

enum TypeContext {
  case ReferenceArgs(attributes: TypeAttributes, fqName: FQName, before: List[Type], after: List[Type])
  case TupleElements(attributes: TypeAttributes, before: List[Type], after: List[Type])
  case RecordFieldType(attributes: TypeAttributes, fieldName: Name, before: List[Field], after: List[Field])
  case ExtensibleRecordFieldType(
      attributes: TypeAttributes,
      variable: Name,
      fieldName: Name,
      before: List[Field],
      after: List[Field]
  )
  case FunctionArgument(attributes: TypeAttributes, returnType: Type)
  case FunctionReturn(attributes: TypeAttributes, argumentType: Type)
  case TypeAliasBody(params: Chunk[Name])
}

final case class TypeCursor(current: Type, ancestors: List[TypeContext]) {
  def up: Option[TypeCursor] = ancestors match {
    case Nil          => None
    case head :: tail =>
      val parent = head match {
        case TypeContext.ReferenceArgs(attributes, name, before, after) =>
          Type.Reference(attributes, name, Chunk.from(before.reverse ::: (current :: after)))
        case TypeContext.TupleElements(attributes, before, after) =>
          Type.Tuple(attributes, Chunk.from(before.reverse ::: (current :: after)))
        case TypeContext.RecordFieldType(attributes, name, before, after) =>
          Type.Record(attributes, Chunk.from(before.reverse ::: (Field(name, current) :: after)))
        case TypeContext.ExtensibleRecordFieldType(attributes, variable, name, before, after) =>
          Type.ExtensibleRecord(
            attributes,
            variable,
            Chunk.from(before.reverse ::: (Field(name, current) :: after))
          )
        case TypeContext.FunctionArgument(attributes, returnType) =>
          Type.Function(attributes, current, returnType)
        case TypeContext.FunctionReturn(attributes, argumentType) =>
          Type.Function(attributes, argumentType, current)
        case _ => ??? // Handle other cases if added
      }
      Some(TypeCursor(parent, tail))
  }

  def down: Option[TypeCursor] = current match {
    case Type.Reference(attributes, name, args) if args.nonEmpty =>
      Some(TypeCursor(args.head, TypeContext.ReferenceArgs(attributes, name, Nil, args.tail.toList) :: ancestors))
    case Type.Tuple(attributes, elements) if elements.nonEmpty =>
      Some(TypeCursor(elements.head, TypeContext.TupleElements(attributes, Nil, elements.tail.toList) :: ancestors))
    case Type.Record(attributes, fields) if fields.nonEmpty =>
      Some(TypeCursor(
        fields.head.fieldType,
        TypeContext.RecordFieldType(attributes, fields.head.name, Nil, fields.tail.toList) :: ancestors
      ))
    case Type.ExtensibleRecord(attributes, variable, fields) if fields.nonEmpty =>
      Some(TypeCursor(
        fields.head.fieldType,
        TypeContext.ExtensibleRecordFieldType(attributes, variable, fields.head.name, Nil, fields.tail.toList) ::
          ancestors
      ))
    case Type.Function(attributes, arg, ret) =>
      Some(TypeCursor(arg, TypeContext.FunctionArgument(attributes, ret) :: ancestors))
    case _ => None
  }

  def left: Option[TypeCursor] = ancestors match {
    case (head: TypeContext) :: tail =>
      head match {
        case TypeContext.ReferenceArgs(attributes, name, before, after) if before.nonEmpty =>
          val prev   = before.head
          val newCtx = TypeContext.ReferenceArgs(attributes, name, before.tail, current :: after)
          Some(TypeCursor(prev, newCtx :: tail))
        case TypeContext.TupleElements(attributes, before, after) if before.nonEmpty =>
          val prev   = before.head
          val newCtx = TypeContext.TupleElements(attributes, before.tail, current :: after)
          Some(TypeCursor(prev, newCtx :: tail))
        case TypeContext.RecordFieldType(attributes, name, before, after) if before.nonEmpty =>
          val prevField = before.head
          val newCtx    =
            TypeContext.RecordFieldType(attributes, prevField.name, before.tail, Field(name, current) :: after)
          Some(TypeCursor(prevField.fieldType, newCtx :: tail))
        case TypeContext.ExtensibleRecordFieldType(attributes, variable, name, before, after) if before.nonEmpty =>
          val prevField = before.head
          val newCtx    = TypeContext.ExtensibleRecordFieldType(
            attributes,
            variable,
            prevField.name,
            before.tail,
            Field(name, current) :: after
          )
          Some(TypeCursor(prevField.fieldType, newCtx :: tail))
        case TypeContext.FunctionReturn(attributes, argumentType) =>
          // Moving left from return type goes to argument type
          Some(TypeCursor(argumentType, TypeContext.FunctionArgument(attributes, current) :: tail))
        case _ => None
      }
    case Nil => None
  }

  def right: Option[TypeCursor] = ancestors match {
    case (head: TypeContext) :: tail =>
      head match {
        case TypeContext.ReferenceArgs(attributes, name, before, after) if after.nonEmpty =>
          val next   = after.head
          val newCtx = TypeContext.ReferenceArgs(attributes, name, current :: before, after.tail)
          Some(TypeCursor(next, newCtx :: tail))
        case TypeContext.TupleElements(attributes, before, after) if after.nonEmpty =>
          val next   = after.head
          val newCtx = TypeContext.TupleElements(attributes, current :: before, after.tail)
          Some(TypeCursor(next, newCtx :: tail))
        case TypeContext.RecordFieldType(attributes, name, before, after) if after.nonEmpty =>
          val nextField = after.head
          val newCtx    =
            TypeContext.RecordFieldType(attributes, nextField.name, Field(name, current) :: before, after.tail)
          Some(TypeCursor(nextField.fieldType, newCtx :: tail))
        case TypeContext.ExtensibleRecordFieldType(attributes, variable, name, before, after) if after.nonEmpty =>
          val nextField = after.head
          val newCtx    = TypeContext.ExtensibleRecordFieldType(
            attributes,
            variable,
            nextField.name,
            Field(name, current) :: before,
            after.tail
          )
          Some(TypeCursor(nextField.fieldType, newCtx :: tail))
        case TypeContext.FunctionArgument(attributes, returnType) =>
          // Moving right from argument type goes to return type
          Some(TypeCursor(returnType, TypeContext.FunctionReturn(attributes, current) :: tail))
        case _ => None
      }
    case Nil => None
  }
}

enum ExprContext {
  case TupleElements(attributes: ValueAttributes, before: List[Expr], after: List[Expr])
  case ListItems(attributes: ValueAttributes, before: List[Expr], after: List[Expr])
  case ApplyFunction(attributes: ValueAttributes, argument: Expr)
  case ApplyArgument(attributes: ValueAttributes, function: Expr)
  case LambdaBody(attributes: ValueAttributes, argumentPattern: Pattern)
  case IfThenElseCondition(attributes: ValueAttributes, thenBranch: Expr, elseBranch: Expr)
  case IfThenElseThenBranch(attributes: ValueAttributes, condition: Expr, elseBranch: Expr)
  case IfThenElseElseBranch(attributes: ValueAttributes, condition: Expr, thenBranch: Expr)
}

final case class ExprCursor(current: Expr, ancestors: List[ExprContext]) {
  def up: Option[ExprCursor] = ancestors match {
    case Nil          => None
    case head :: tail =>
      val parent = head match {
        case ExprContext.TupleElements(attributes, before, after) =>
          Expr.Tuple(attributes, Chunk.from(before.reverse ::: (current :: after)))
        case ExprContext.ListItems(attributes, before, after) =>
          Expr.List(attributes, Chunk.from(before.reverse ::: (current :: after)))
        case ExprContext.ApplyFunction(attributes, argument) =>
          Expr.Apply(attributes, current, argument)
        case ExprContext.ApplyArgument(attributes, function) =>
          Expr.Apply(attributes, function, current)
        case ExprContext.LambdaBody(attributes, argumentPattern) =>
          Expr.Lambda(attributes, argumentPattern, current)
        case ExprContext.IfThenElseCondition(attributes, thenBranch, elseBranch) =>
          Expr.IfThenElse(attributes, current, thenBranch, elseBranch)
        case ExprContext.IfThenElseThenBranch(attributes, condition, elseBranch) =>
          Expr.IfThenElse(attributes, condition, current, elseBranch)
        case ExprContext.IfThenElseElseBranch(attributes, condition, thenBranch) =>
          Expr.IfThenElse(attributes, condition, thenBranch, current)
      }
      Some(ExprCursor(parent, tail))
  }

  def down: Option[ExprCursor] = current match {
    case Expr.Tuple(attributes, elements) if elements.nonEmpty =>
      Some(ExprCursor(elements.head, ExprContext.TupleElements(attributes, Nil, elements.tail.toList) :: ancestors))
    case Expr.List(attributes, items) if items.nonEmpty =>
      Some(ExprCursor(items.head, ExprContext.ListItems(attributes, Nil, items.tail.toList) :: ancestors))
    case Expr.Apply(attributes, function, argument) =>
      Some(ExprCursor(function, ExprContext.ApplyFunction(attributes, argument) :: ancestors))
    case Expr.Lambda(attributes, pattern, body) =>
      Some(ExprCursor(body, ExprContext.LambdaBody(attributes, pattern) :: ancestors))
    case Expr.IfThenElse(attributes, condition, thenBranch, elseBranch) =>
      Some(ExprCursor(condition, ExprContext.IfThenElseCondition(attributes, thenBranch, elseBranch) :: ancestors))
    case _ => None
  }
  def left: Option[ExprCursor] = ancestors match {
    case (head: ExprContext) :: tail =>
      head match {
        case ExprContext.TupleElements(attributes, before, after) if before.nonEmpty =>
          val prev   = before.head
          val newCtx = ExprContext.TupleElements(attributes, before.tail, current :: after)
          Some(ExprCursor(prev, newCtx :: tail))
        case ExprContext.ListItems(attributes, before, after) if before.nonEmpty =>
          val prev   = before.head
          val newCtx = ExprContext.ListItems(attributes, before.tail, current :: after)
          Some(ExprCursor(prev, newCtx :: tail))
        case ExprContext.ApplyArgument(attributes, function) =>
          // Left of argument is function
          Some(ExprCursor(function, ExprContext.ApplyFunction(attributes, current) :: tail))
        case ExprContext.IfThenElseThenBranch(attributes, condition, elseBranch) =>
          // Left of thenBranch is condition
          Some(ExprCursor(condition, ExprContext.IfThenElseCondition(attributes, current, elseBranch) :: tail))
        case ExprContext.IfThenElseElseBranch(attributes, condition, thenBranch) =>
          // Left of elseBranch is thenBranch
          Some(ExprCursor(thenBranch, ExprContext.IfThenElseThenBranch(attributes, condition, current) :: tail))
        case _ => None
      }
    case Nil => None
  }

  def right: Option[ExprCursor] = ancestors match {
    case (head: ExprContext) :: tail =>
      head match {
        case ExprContext.TupleElements(attributes, before, after) if after.nonEmpty =>
          val next   = after.head
          val newCtx = ExprContext.TupleElements(attributes, current :: before, after.tail)
          Some(ExprCursor(next, newCtx :: tail))
        case ExprContext.ListItems(attributes, before, after) if after.nonEmpty =>
          val next   = after.head
          val newCtx = ExprContext.ListItems(attributes, current :: before, after.tail)
          Some(ExprCursor(next, newCtx :: tail))
        case ExprContext.ApplyFunction(attributes, argument) =>
          // Right of function is argument
          Some(ExprCursor(argument, ExprContext.ApplyArgument(attributes, current) :: tail))
        case ExprContext.IfThenElseCondition(attributes, thenBranch, elseBranch) =>
          // Right of condition is thenBranch
          Some(ExprCursor(thenBranch, ExprContext.IfThenElseThenBranch(attributes, current, elseBranch) :: tail))
        case ExprContext.IfThenElseThenBranch(attributes, condition, elseBranch) =>
          // Right of thenBranch is elseBranch
          Some(ExprCursor(elseBranch, ExprContext.IfThenElseElseBranch(attributes, condition, current) :: tail))
        case _ => None
      }
    case Nil => None
  }
}

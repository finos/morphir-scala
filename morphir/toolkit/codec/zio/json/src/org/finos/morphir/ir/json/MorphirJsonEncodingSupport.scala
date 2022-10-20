package org.finos.morphir.ir.json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write
import org.finos.morphir.ir.AccessControlled.Access._
import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, Specification => PackageSpecification}
import org.finos.morphir.ir.Type.{
  Constructors,
  Definition => TypeDefinition,
  Field,
  Specification => TypeSpecification,
  Type
}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir.Value.{Value, _}
import org.finos.morphir.ir.module.{
  Definition => ModuleDefinition,
  ModuleName,
  ModulePath,
  Specification => ModuleSpecification
}
import org.finos.morphir.ir.internal.types.Type
import org.finos.morphir.ir.value.recursive.ValueCase
import org.finos.morphir.ir.{Literal, _}

trait MorphirJsonEncodingSupport {
  implicit val unitEncoder: JsonEncoder[Unit] = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val pathEncoder: JsonEncoder[Path] = JsonEncoder.list[Name].contramap(path => path.segments.toList)
  implicit val modulePathEncoder: JsonEncoder[ModulePath]   = pathEncoder.contramap(_.toPath)
  implicit val packageNameEncoder: JsonEncoder[PackageName] = pathEncoder.contramap(_.toPath)
  implicit val qNameEncoder: JsonEncoder[QName] =
    Json.encoder.contramap[QName](qName =>
      Json.Arr(toJsonAstOrThrow(qName.modulePath), toJsonAstOrThrow(qName.localName))
    )

  implicit val fqNameEncoder: JsonEncoder[FQName] =
    Json.encoder.contramap[FQName](fqName =>
      Json.Arr(
        toJsonAstOrThrow(fqName.packagePath),
        toJsonAstOrThrow(fqName.modulePath),
        toJsonAstOrThrow(fqName.localName)
      )
    )

  implicit val moduleNameEncoder: JsonEncoder[ModuleName] =
    Json.encoder.contramap[ModuleName](moduleName =>
      Json.Arr(toJsonAstOrThrow(moduleName.namespace), toJsonAstOrThrow(moduleName.localName))
    )

  implicit def fieldEncoder[A: JsonEncoder]: JsonEncoder[Field[A]] =
    Json.encoder.contramap[Field[A]](field =>
      Json.Obj(
        "name" -> toJsonAstOrThrow(field.name),
        "tpe"  -> toJsonAstOrThrow(field.data)
      )
    )

  implicit def literalBoolEncoder: JsonEncoder[Literal.Bool] = Json.encoder.contramap[Literal.Bool] { literal =>
    Json.Arr(Json.Str("BoolLiteral"), Json.Bool(literal.value))
  }

  implicit def literalCharEncoder: JsonEncoder[Literal.Char] = Json.encoder.contramap[Literal.Char] { literal =>
    Json.Arr(Json.Str("CharLiteral"), Json.Str(literal.value.toString))
  }

  implicit def literalFloatEncoder: JsonEncoder[Literal.Float] = Json.encoder.contramap[Literal.Float] { literal =>
    Json.Arr(Json.Str("FloatLiteral"), Json.Num(literal.value))
  }

  implicit def literalStringEncoder: JsonEncoder[Literal.String] = Json.encoder.contramap[Literal.String] { literal =>
    Json.Arr(Json.Str("StringLiteral"), Json.Str(literal.value))
  }

  implicit def literalWholeNumberEncoder: JsonEncoder[Literal.WholeNumber] =
    Json.encoder.contramap[Literal.WholeNumber] { literal =>
      Json.Arr(Json.Str("WholeNumberLiteral"), Json.Num(new java.math.BigDecimal(literal.value)))
    }

  implicit def literalEncoder: JsonEncoder[Literal[Any]] =
    new JsonEncoder[Literal[Any]] {
      def unsafeEncode(a: Literal[Any], indent: Option[Int], out: Write): Unit = a match {
        case literalBool: Literal.Bool     => literalBoolEncoder.unsafeEncode(literalBool, indent, out)
        case literalChar: Literal.Char     => literalCharEncoder.unsafeEncode(literalChar, indent, out)
        case literalFloat: Literal.Float   => literalFloatEncoder.unsafeEncode(literalFloat, indent, out)
        case literalString: Literal.String => literalStringEncoder.unsafeEncode(literalString, indent, out)
        case literalWholeNumber: Literal.WholeNumber =>
          literalWholeNumberEncoder.unsafeEncode(literalWholeNumber, indent, out)
      }
    }

  implicit def patternAsPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.AsPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Pattern[Attributes], Name].contramap {
      case Pattern.AsPattern(pattern, name, attributes) =>
        ("as_pattern", attributes, pattern, name)
    }

  implicit def patternConstructorPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.ConstructorPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, FQName, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.ConstructorPattern(constructorName, argumentPatterns, attributes) =>
        ("constructor_pattern", attributes, constructorName, argumentPatterns)
    }

  implicit def patternEmptyListPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.EmptyListPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.EmptyListPattern(attributes) =>
      ("empty_list_pattern", attributes)
    }

  implicit def patternHeadTailPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.HeadTailPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Pattern[Attributes], Pattern[Attributes]].contramap {
      case Pattern.HeadTailPattern(headPattern, tailPattern, attributes) =>
        ("head_tail_pattern", attributes, headPattern, tailPattern)
    }

  implicit def patternLiteralPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.LiteralPattern[Any, Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Literal[Any]].contramap { case Pattern.LiteralPattern(literal, attributes) =>
      ("literal_pattern", attributes, literal)
    }

  implicit def patternTuplePatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.TuplePattern[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.TuplePattern(elementPatterns, attributes) =>
        ("tuple_pattern", attributes, elementPatterns)
    }

  implicit def patternUnitPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.UnitPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.UnitPattern(attributes) =>
      ("unit_pattern", attributes)
    }

  implicit def patternWildcardPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.WildcardPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.WildcardPattern(attributes) =>
      ("wildcard_pattern", attributes)
    }

  implicit def patternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern[Attributes]] =
    new JsonEncoder[Pattern[Attributes]] {
      def unsafeEncode(pattern: Pattern[Attributes], indent: Option[Int], out: Write): Unit = pattern match {
        case pattern @ Pattern.AsPattern(_, _, _) =>
          JsonEncoder[Pattern.AsPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.ConstructorPattern(_, _, _) =>
          JsonEncoder[Pattern.ConstructorPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.EmptyListPattern(_) =>
          JsonEncoder[Pattern.EmptyListPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.HeadTailPattern(_, _, _) =>
          JsonEncoder[Pattern.HeadTailPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.LiteralPattern(_, _) =>
          JsonEncoder[Pattern.LiteralPattern[Any, Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.TuplePattern(_, _) =>
          JsonEncoder[Pattern.TuplePattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.UnitPattern(_) =>
          JsonEncoder[Pattern.UnitPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.WildcardPattern(_) =>
          JsonEncoder[Pattern.WildcardPattern[Attributes]].unsafeEncode(pattern, indent, out)
      }
    }

  implicit def constructorsEncoder[Attributes: JsonEncoder]: JsonEncoder[Constructors[Attributes]] =
    Json.encoder.contramap[Constructors[Attributes]] { ctors =>
      toJsonAstOrThrow(
        ctors.toMap.toList.map { case (ctorName: Name, ctorArgs: Chunk[(Name, Type[Attributes])]) =>
          (
            toJsonAstOrThrow(ctorName),
            toJsonAstOrThrow(
              ctorArgs.map { case (argName: Name, argType: Type[Attributes]) =>
                Json.Arr(toJsonAstOrThrow(argName), toJsonAstOrThrow(argType))
              }
            )
          )
        }
      )
    }

  implicit def accessEncoder: JsonEncoder[AccessControlled.Access] =
    Json.encoder.contramap[AccessControlled.Access] { access =>
      access match {
        case Public  => Json.Str("Public")
        case Private => Json.Str("Private")
      }
    }

  implicit def accessControlledEncoder[A: JsonEncoder]: JsonEncoder[AccessControlled[A]] =
    Json.encoder.contramap[AccessControlled[A]] { accessControlled =>
      Json.Obj(
        "access" -> toJsonAstOrThrow(accessControlled.access),
        "value"  -> toJsonAstOrThrow(accessControlled.value)
      )
    }

  implicit def typeDefinitionTypeAliasEncoder[A: JsonEncoder]: JsonEncoder[TypeDefinition.TypeAlias[A]] =
    Json.encoder.contramap[TypeDefinition.TypeAlias[A]] { alias =>
      Json.Arr(Json.Str("TypeAliasDefinition"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.typeExp))
    }

  implicit def typeDefinitionCustomTypeEncoder[A: JsonEncoder]: JsonEncoder[TypeDefinition.CustomType[A]] =
    Json.encoder.contramap[TypeDefinition.CustomType[A]] { tpe =>
      Json.Arr(Json.Str("CustomTypeDefinition"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeDefinitionEncoder[Attributes: JsonEncoder]: JsonEncoder[TypeDefinition[Attributes]] =
    new JsonEncoder[TypeDefinition[Attributes]] {
      def unsafeEncode(d: TypeDefinition[Attributes], indent: Option[Int], out: Write): Unit = d match {
        case d @ TypeDefinition.TypeAlias(_, _) =>
          JsonEncoder[TypeDefinition.TypeAlias[Attributes]].unsafeEncode(d, indent, out)
        case d @ TypeDefinition.CustomType(_, _) =>
          JsonEncoder[TypeDefinition.CustomType[Attributes]].unsafeEncode(d, indent, out)
      }
    }

  implicit def typeSpecificationTypeAliasEncoder[Attributes: JsonEncoder]
      : JsonEncoder[TypeSpecification.TypeAliasSpecification[Attributes]] =
    Json.encoder.contramap[TypeSpecification.TypeAliasSpecification[Attributes]] { alias =>
      Json.Arr(Json.Str("TypeAliasSpecification"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.expr))
    }

  implicit def typeSpecificationEncoderCustomTypeEncoder[Attributes: JsonEncoder]
      : JsonEncoder[TypeSpecification.CustomTypeSpecification[Attributes]] =
    Json.encoder.contramap[TypeSpecification.CustomTypeSpecification[Attributes]] { tpe =>
      Json.Arr(Json.Str("CustomTypeSpecification"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeSpecificationEncoderOpaqueTypeEncoder2: JsonEncoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonEncoder.tuple2[String, Chunk[Name]].contramap {
      case TypeSpecification.OpaqueTypeSpecification(typeParams: Chunk[Name]) =>
        ("OpaqueTypeSpecification", typeParams)
    }

  implicit def typeSpecificationEncoder[Attributes: JsonEncoder]: JsonEncoder[TypeSpecification[Attributes]] =
    new JsonEncoder[TypeSpecification[Attributes]] {
      def unsafeEncode(spec: TypeSpecification[Attributes], indent: Option[Int], out: Write): Unit =
        spec match {
          case spec @ TypeSpecification.TypeAliasSpecification(_, _) =>
            JsonEncoder[TypeSpecification.TypeAliasSpecification[Attributes]].unsafeEncode(spec, indent, out)
          case spec @ TypeSpecification.CustomTypeSpecification(_, _) =>
            JsonEncoder[TypeSpecification.CustomTypeSpecification[Attributes]].unsafeEncode(spec, indent, out)
          case spec @ TypeSpecification.OpaqueTypeSpecification(_) =>
            JsonEncoder[TypeSpecification.OpaqueTypeSpecification].unsafeEncode(spec, indent, out)
        }
    }

  implicit def valueDefinitionEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[ValueDefinition[TA, VA]] =
    Json.encoder.contramap[ValueDefinition[TA, VA]] { definition =>
      Json.Obj(
        "inputTypes" -> toJsonAstOrThrow(definition.inputTypes),
        "outputType" -> toJsonAstOrThrow(definition.outputType),
        "body"       -> toJsonAstOrThrow(definition.body)
      )
    }

  // final case class Case[+TA, +VA, +TypeRepr[+_]: Covariant, +Z](
  //     inputTypes: Chunk[(Name, VA, TypeRepr[TA])],
  //     outputType: TypeRepr[TA],
  //     body: Z
  // )
  implicit def valueDefinitionCaseEncoder[TA: JsonEncoder, VA: JsonEncoder, Z: JsonEncoder]
      : JsonEncoder[ValueDefinition.Case[TA, VA, Type, Z]] =
    Json.encoder.contramap[ValueDefinition.Case[TA, VA, Type, Z]] { definition =>
      Json.Obj(
        "inputTypes" -> toJsonAstOrThrow(definition.inputTypes),
        "outputType" -> toJsonAstOrThrow(definition.outputType),
        "body"       -> toJsonAstOrThrow(definition.body)
      )
    }

  implicit def anyEncoder: JsonEncoder[Any] =
    Json.Null.encoder.contramap(_ => Json.Null)

  implicit def valueSpecificationEncoder[A: JsonEncoder]: JsonEncoder[ValueSpecification[A]] =
    Json.encoder.contramap[ValueSpecification[A]] { specification =>
      Json.Obj(
        "inputs"  -> toJsonAstOrThrow(specification.inputs),
        "outputs" -> toJsonAstOrThrow(specification.output)
      )
    }

  //   final case class ApplyCase[+VA, +Self](attributes: VA, function: Self, argument: Self) extends ValueCase[Nothing, VA, Self]
  implicit def ApplyCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.ApplyCase[VA, Self]] =
    JsonEncoder.tuple4[String, VA, Self, Self].contramap { case ValueCase.ApplyCase(attributes, function, argument) =>
      ("apply", attributes, function, argument)
    }

  //   final case class ConstructorCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]
  implicit def ConstructorCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.ConstructorCase[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case ValueCase.ConstructorCase(attributes, name) =>
      ("constructor", attributes, name)
    }

  //   final case class DestructureCase[+VA, +Self](attributes: VA, pattern: Pattern[VA], valueToDestruct: Self, inValue: Self) extends ValueCase[Nothing, VA, Self]
  implicit def DestructureCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.DestructureCase[VA, Self]] =
    JsonEncoder.tuple5[String, VA, Pattern[VA], Self, Self].contramap {
      case ValueCase.DestructureCase(attributes, pattern, valueToDestruct, inValue) =>
        ("destructure", attributes, pattern, valueToDestruct, inValue)
    }

  //   final case class FieldCase[+VA, +Self](attributes: VA, target: Self, name: Name) extends ValueCase[Nothing, VA, Self]
  implicit def FieldCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.FieldCase[VA, Self]] =
    JsonEncoder.tuple4[String, VA, Self, Name].contramap { case ValueCase.FieldCase(attributes, target, name) =>
      ("field", attributes, target, name)
    }

  //   final case class FieldFunctionCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]
  implicit def FieldFunctionCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.FieldFunctionCase[VA]] =
    JsonEncoder.tuple3[String, VA, Name].contramap { case ValueCase.FieldFunctionCase(attributes, name) =>
      ("field_function", attributes, name)
    }

  //   final case class IfThenElseCase[+VA, +Self](attributes: VA, condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Nothing, VA, Self]
  implicit def IfThenElseCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.IfThenElseCase[VA, Self]] =
    JsonEncoder.tuple5[String, VA, Self, Self, Self].contramap {
      case ValueCase.IfThenElseCase(attributes, condition, thenBranch, elseBranch) =>
        ("if_then_else", attributes, condition, thenBranch, elseBranch)
    }

  //   final case class LambdaCase[+VA, +Self](attributes: VA, argumentPattern: Pattern[VA], body: Self) extends ValueCase[Nothing, VA, Self]
  implicit def LambdaCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.LambdaCase[VA, Self]] =
    JsonEncoder.tuple4[String, VA, Pattern[VA], Self].contramap {
      case ValueCase.LambdaCase(attributes, argumentPattern, body) => ("lambda", attributes, argumentPattern, body)
    }

  // final case class LetDefinitionCase[+TA, +VA, +TypeRepr[+_], +Self](
  //     attributes: VA,
  //     valueName: Name,
  //     valueDefinition: Definition.Case[TA, VA, TypeRepr, Self],
  //     inValue: Self
  // ) extends ValueCase[TA, VA, Self]
  implicit def LetDefinitionCaseValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.LetDefinitionCase[TA, VA, Type, Self]] =
    JsonEncoder.tuple5[String, VA, Name, ValueDefinition.Case[TA, VA, Type, Self], Self].contramap {
      case ValueCase.LetDefinitionCase(attributes, valueName, valueDefinition, inValue) =>
        ("let_definition", attributes, valueName, valueDefinition, inValue)
    }

  // final case class LetRecursionCase[+TA, +VA, +TypeRepr[+_], +Self](
  //     attributes: VA,
  //     valueDefinitions: Map[Name, Definition.Case[TA, VA, TypeRepr, Self]],
  //     inValue: Self
  // ) extends ValueCase[TA, VA, Self]
  implicit def LetRecursionCaseValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.LetRecursionCase[TA, VA, Type, Self]] =
    JsonEncoder.tuple4[String, VA, List[(Name, ValueDefinition.Case[TA, VA, Type, Self])], Self].contramap {
      case ValueCase.LetRecursionCase(attributes, valueDefinitions, inValue) =>
        ("let_recursion", attributes, valueDefinitions.toList, inValue)
    }

  //   final case class ListCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]
  implicit def ListCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]: JsonEncoder[ValueCase.ListCase[VA, Self]] =
    JsonEncoder.tuple3[String, VA, Chunk[Self]].contramap { case ValueCase.ListCase(attributes, elements) =>
      ("list", attributes, elements)
    }

  //   final case class LiteralCase[+VA, +A](attributes: VA, literal: Literal[A]) extends ValueCase[Nothing, VA, Nothing]
  implicit def LiteralCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.LiteralCase[VA, Any]] =
    JsonEncoder.tuple3[String, VA, Literal[Any]].contramap { case ValueCase.LiteralCase(attributes, literal) =>
      ("literal", attributes, literal)
    }

  //   final case class PatternMatchCase[+VA, +Self](attributes: VA, branchOutOn: Self, cases: Chunk[(Pattern[VA], Self)]) extends ValueCase[Nothing, VA, Self]
  implicit def PatternMatchCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.PatternMatchCase[VA, Self]] =
    JsonEncoder.tuple4[String, VA, Self, Chunk[(Pattern[VA], Self)]].contramap {
      case ValueCase.PatternMatchCase(attributes, branchOutOn, cases) =>
        ("pattern_match", attributes, branchOutOn, cases)
    }

  // final case class RecordCase[+VA, +Self](attributes: VA, fields: Chunk[(Name, Self)])
  implicit def RecordCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.RecordCase[VA, Self]] =
    JsonEncoder.tuple3[String, VA, Chunk[(Name, Self)]].contramap { case ValueCase.RecordCase(attributes, fields) =>
      ("record", attributes, fields)
    }

  //   final case class ReferenceCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]
  implicit def ReferenceCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.ReferenceCase[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case ValueCase.ReferenceCase(attributes, name) =>
      ("reference", attributes, name)
    }

  //   final case class TupleCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]
  implicit def TupleCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.TupleCase[VA, Self]] =
    JsonEncoder.tuple3[String, VA, Chunk[Self]].contramap { case ValueCase.TupleCase(attributes, elements) =>
      ("Tuple", attributes, elements)
    }

  //   final case class UpdateRecordCase[+VA, +Self](attributes: VA, valueToUpdate: Self, fieldsToUpdate: Chunk[(Name, Self)]) extends ValueCase[Nothing, VA, Self]
  implicit def UpdateRecordCaseValueJsonEncoder[VA: JsonEncoder, Self: JsonEncoder]
      : JsonEncoder[ValueCase.UpdateRecordCase[VA, Self]] =
    JsonEncoder.tuple4[String, VA, Self, Chunk[(Name, Self)]].contramap {
      case ValueCase.UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate) =>
        ("update_record", attributes, valueToUpdate, fieldsToUpdate)
    }

  implicit def UnitCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.UnitCase[VA]] =
    JsonEncoder.tuple2[String, VA].contramap { case ValueCase.UnitCase(attributes) =>
      ("unit", attributes)
    }

  //   final case class VariableCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]
  implicit def VariableCaseValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[ValueCase.VariableCase[VA]] =
    JsonEncoder.tuple3[String, VA, Name].contramap { case ValueCase.VariableCase(attributes, name) =>
      ("variable", attributes, name)
    }

  implicit def valueEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value[TA, VA]] =
    new JsonEncoder[Value[TA, VA]] {
      def unsafeEncode(value: Value[TA, VA], indent: Option[Int], out: Write): Unit = value.caseValue match {
        case t @ ValueCase.ApplyCase(_, _, _) =>
          JsonEncoder[ValueCase.ApplyCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.ConstructorCase(_, _) =>
          JsonEncoder[ValueCase.ConstructorCase[VA]].unsafeEncode(t, indent, out)
        case t @ ValueCase.DestructureCase(_, _, _, _) =>
          JsonEncoder[ValueCase.DestructureCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.FieldCase(_, _, _) =>
          JsonEncoder[ValueCase.FieldCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.FieldFunctionCase(_, _) =>
          JsonEncoder[ValueCase.FieldFunctionCase[VA]].unsafeEncode(t, indent, out)
        case t @ ValueCase.IfThenElseCase(_, _, _, _) =>
          JsonEncoder[ValueCase.IfThenElseCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.LambdaCase(_, _, _) =>
          JsonEncoder[ValueCase.LambdaCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.LetDefinitionCase(_, _, _, _) =>
          LetDefinitionCaseValueJsonEncoder[TA, VA, Value[TA, VA]].unsafeEncode(
            t.asInstanceOf[ValueCase.LetDefinitionCase[TA, VA, Type, Value[TA, VA]]],
            indent,
            out
          )
        case t @ ValueCase.LetRecursionCase(_, _, _) =>
          LetRecursionCaseValueJsonEncoder[TA, VA, Value[TA, VA]].unsafeEncode(
            t.asInstanceOf[ValueCase.LetRecursionCase[TA, VA, Type, Value[TA, VA]]],
            indent,
            out
          )
        case t @ ValueCase.ListCase(_, _) =>
          JsonEncoder[ValueCase.ListCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.LiteralCase(_, _) =>
          JsonEncoder[ValueCase.LiteralCase[VA, Any]].unsafeEncode(t, indent, out)
        case t @ ValueCase.PatternMatchCase(_, _, _) =>
          JsonEncoder[ValueCase.PatternMatchCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.RecordCase(_, _) =>
          JsonEncoder[ValueCase.RecordCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.ReferenceCase(_, _) =>
          JsonEncoder[ValueCase.ReferenceCase[VA]].unsafeEncode(t, indent, out)
        case t @ ValueCase.TupleCase(_, _) =>
          JsonEncoder[ValueCase.TupleCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.UpdateRecordCase(_, _, _) =>
          JsonEncoder[ValueCase.UpdateRecordCase[VA, Value[TA, VA]]].unsafeEncode(t, indent, out)
        case t @ ValueCase.UnitCase(_) =>
          JsonEncoder[ValueCase.UnitCase[VA]].unsafeEncode(t, indent, out)
        case t @ ValueCase.VariableCase(_, _) =>
          JsonEncoder[ValueCase.VariableCase[VA]].unsafeEncode(t, indent, out)
      }
    }

  implicit def ExtensibleRecordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.ExtensibleRecord[A]] =
    JsonEncoder.tuple4[String, A, Name, Chunk[Field[Type[A]]]].contramap {
      case Type.ExtensibleRecord(attributes, name, fields) => ("ExtensibleRecord", attributes, name, fields)
    }

  implicit def FunctionTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Function[A]] =
    JsonEncoder.tuple4[String, A, Type[A], Type[A]].contramap {
      case Type.Function(attributes, argumentType, returnType) =>
        ("Function", attributes, argumentType, returnType)
    }

  implicit def RecordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Record[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Field[Type[A]]]].contramap { case Type.Record(attributes, fields) =>
      ("Record", attributes, fields)
    }

  implicit def ReferenceTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Reference[A]] =
    JsonEncoder.tuple4[String, A, FQName, Chunk[Type[A]]].contramap {
      case Type.Reference(attributes, typeName, typeParams) =>
        ("Reference", attributes, typeName, typeParams)
    }

  implicit def TupleTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Tuple[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Type[A]]].contramap { case Type.Tuple(attributes, elements) =>
      ("Tuple", attributes, elements)
    }

  implicit def UnitTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Unit[A]] =
    JsonEncoder.tuple2[String, A].contramap[Type.Unit[A]] { case Type.Unit(attributes) =>
      ("Unit", attributes)
    }

  implicit def VariableTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Variable[A]] =
    JsonEncoder.tuple3[String, A, Name].contramap[Type.Variable[A]] { case Type.Variable(attributes, name) =>
      ("Variable", attributes, name)
    }

  implicit def typeEncoder[A: JsonEncoder]: JsonEncoder[Type[A]] =
    new JsonEncoder[Type[A]] {
      def unsafeEncode(tpe: Type[A], indent: Option[Int], out: Write): Unit = tpe match {
        case t @ Type.ExtensibleRecord(_, _, _) =>
          JsonEncoder[Type.ExtensibleRecord[A]].unsafeEncode(t, indent, out)
        case t @ Type.Function(_, _, _) =>
          JsonEncoder[Type.Function[A]].unsafeEncode(t, indent, out)
        case t @ Type.Record(_, _) =>
          JsonEncoder[Type.Record[A]].unsafeEncode(t, indent, out)
        case t @ Type.Reference(_, _, _) =>
          JsonEncoder[Type.Reference[A]].unsafeEncode(t, indent, out)
        case t @ Type.Tuple(_, _) =>
          JsonEncoder[Type.Tuple[A]].unsafeEncode(t, indent, out)
        case t @ Type.Unit(_) =>
          JsonEncoder[Type.Unit[A]].unsafeEncode(t, indent, out)
        case t @ Type.Variable(_, _) =>
          JsonEncoder[Type.Variable[A]].unsafeEncode(t, indent, out)
      }
    }

  implicit def documentedEncoder[A: JsonEncoder]: JsonEncoder[Documented[A]] = {
    lazy val encoder: JsonEncoder[Documented[A]] = DeriveJsonEncoder.gen
    encoder
  }

  implicit def moduleSpecificationEncoder[TA: JsonEncoder]: JsonEncoder[ModuleSpecification[TA]] =
    Json.encoder.contramap[ModuleSpecification[TA]] { specification =>
      Json.Obj(
        "types"  -> toJsonAstOrThrow(specification.types.toList),
        "values" -> toJsonAstOrThrow(specification.values.toList)
      )
    }

  implicit def moduleDefinitionEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[ModuleDefinition[TA, VA]] =
    Json.encoder.contramap[ModuleDefinition[TA, VA]] { definition =>
      Json.Obj(
        "types"  -> toJsonAstOrThrow(definition.types.toList),
        "values" -> toJsonAstOrThrow(definition.values.toList)
      )
    }

  implicit def packageSpecificationEncoder[TA: JsonEncoder]: JsonEncoder[PackageSpecification[TA]] =
    Json.encoder.contramap[PackageSpecification[TA]] { specification =>
      Json.Obj(
        "modules" -> toJsonAstOrThrow(specification.modules.toList.map { case (name, moduleSpecification) =>
          Json.Arr(toJsonAstOrThrow(name), toJsonAstOrThrow(moduleSpecification))
        })
      )
    }

  implicit def packageDefinitionEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[PackageDefinition[TA, VA]] =
    Json.encoder.contramap[PackageDefinition[TA, VA]] { definition =>
      Json.Obj(
        "modules" -> toJsonAstOrThrow(definition.modules.toList.map { case (name, moduleDefinition) =>
          Json.Arr(toJsonAstOrThrow(name), toJsonAstOrThrow(moduleDefinition))
        })
      )
    }

  private def toJsonAstOrThrow[A](a: A)(implicit encoder: JsonEncoder[A]): Json =
    a.toJsonAST.toOption.get
}

object MorphirJsonEncodingSupport extends MorphirJsonEncodingSupport

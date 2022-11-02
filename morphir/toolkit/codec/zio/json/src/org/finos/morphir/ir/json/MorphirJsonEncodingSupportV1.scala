package org.finos.morphir.ir
package json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write
import org.finos.morphir.ir.AccessControlled.Access._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, Specification => PackageSpecification}
import org.finos.morphir.ir.Type.{Constructors, Definition => TypeDefinition, Specification => TypeSpecification, Type}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir.Value.{Value, _}
import org.finos.morphir.ir.module.{
  Definition => ModuleDefinition,
  ModuleName,
  ModulePath,
  Specification => ModuleSpecification
}

trait MorphirJsonEncodingSupportV1 {
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
    Json.encoder.contramap[Field[A]](field => Json.Arr(toJsonAstOrThrow(field.name), toJsonAstOrThrow(field.data)))

  implicit def literalBoolEncoder: JsonEncoder[BoolLiteral] = Json.encoder.contramap[BoolLiteral] { literal =>
    Json.Arr(Json.Str("bool_literal"), Json.Bool(literal.value))
  }

  implicit def literalCharEncoder: JsonEncoder[CharLiteral] = Json.encoder.contramap[CharLiteral] { literal =>
    Json.Arr(Json.Str("char_literal"), Json.Str(literal.value.toString))
  }

  implicit def literalDecimalEncoder: JsonEncoder[DecimalLiteral] = Json.encoder.contramap[DecimalLiteral] { literal =>
    Json.Arr(Json.Str("decimal_literal"), Json.Str(literal.value.toString))
  }

  implicit def literalFloatEncoder: JsonEncoder[FloatLiteral] = Json.encoder.contramap[FloatLiteral] { literal =>
    Json.Arr(Json.Str("float_literal"), Json.Num(literal.value))
  }

  implicit def literalStringEncoder: JsonEncoder[StringLiteral] = Json.encoder.contramap[StringLiteral] { literal =>
    Json.Arr(Json.Str("string_literal"), Json.Str(literal.value))
  }

  implicit def literalWholeNumberEncoder: JsonEncoder[WholeNumberLiteral] =
    Json.encoder.contramap[WholeNumberLiteral] { literal =>
      Json.Arr(Json.Str("int_literal"), Json.Num(literal.value))
    }

  implicit def literalEncoder: JsonEncoder[Literal] =
    new JsonEncoder[Literal] {
      def unsafeEncode(a: Literal, indent: Option[Int], out: Write): Unit = a match {
        case literalBool: BoolLiteral       => literalBoolEncoder.unsafeEncode(literalBool, indent, out)
        case literalChar: CharLiteral       => literalCharEncoder.unsafeEncode(literalChar, indent, out)
        case literalDecimal: DecimalLiteral => literalDecimalEncoder.unsafeEncode(literalDecimal, indent, out)
        case literalFloat: FloatLiteral     => literalFloatEncoder.unsafeEncode(literalFloat, indent, out)
        case literalString: StringLiteral   => literalStringEncoder.unsafeEncode(literalString, indent, out)
        case literalWholeNumber: WholeNumberLiteral =>
          literalWholeNumberEncoder.unsafeEncode(literalWholeNumber, indent, out)
      }
    }

  implicit def patternAsPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.AsPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Pattern[Attributes], Name].contramap {
      case Pattern.AsPattern(attributes, pattern, name) =>
        ("as_pattern", attributes, pattern, name)
    }

  implicit def patternConstructorPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.ConstructorPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, FQName, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns) =>
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
      case Pattern.HeadTailPattern(attributes, headPattern, tailPattern) =>
        ("head_tail_pattern", attributes, headPattern, tailPattern)
    }

  implicit def patternLiteralPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.LiteralPattern[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Literal].contramap { case Pattern.LiteralPattern(attributes, literal) =>
      ("literal_pattern", attributes, literal)
    }

  implicit def patternTuplePatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.TuplePattern[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.TuplePattern(attributes, elementPatterns) =>
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
          JsonEncoder[Pattern.LiteralPattern[Attributes]].unsafeEncode(pattern, indent, out)
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

  implicit def accessControlledEncoder[A: JsonEncoder]: JsonEncoder[AccessControlled[A]] =
    Json.encoder.contramap[AccessControlled[A]] { accessControlled =>
      accessControlled.access match {
        case Public  => Json.Arr(Json.Str("public"), toJsonAstOrThrow(accessControlled.value))
        case Private => Json.Arr(Json.Str("private"), toJsonAstOrThrow(accessControlled.value))
      }
    }

  implicit def typeDefinitionTypeAliasEncoder[A: JsonEncoder]: JsonEncoder[TypeDefinition.TypeAlias[A]] =
    Json.encoder.contramap[TypeDefinition.TypeAlias[A]] { alias =>
      Json.Arr(Json.Str("type_alias_definition"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.typeExp))
    }

  implicit def typeDefinitionCustomTypeEncoder[A: JsonEncoder]: JsonEncoder[TypeDefinition.CustomType[A]] =
    Json.encoder.contramap[TypeDefinition.CustomType[A]] { tpe =>
      Json.Arr(Json.Str("custom_type_definition"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
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
      Json.Arr(Json.Str("type_alias_specification"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.expr))
    }

  implicit def typeSpecificationEncoderCustomTypeEncoder[Attributes: JsonEncoder]
      : JsonEncoder[TypeSpecification.CustomTypeSpecification[Attributes]] =
    Json.encoder.contramap[TypeSpecification.CustomTypeSpecification[Attributes]] { tpe =>
      Json.Arr(Json.Str("custom_type_specification"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeSpecificationEncoderOpaqueTypeEncoder2: JsonEncoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonEncoder.tuple2[String, Chunk[Name]].contramap {
      case TypeSpecification.OpaqueTypeSpecification(typeParams: Chunk[Name]) =>
        ("opaque_type_specification", typeParams)
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

  // implicit def anyEncoder: JsonEncoder[Any] =
  //   Json.Null.encoder.contramap(_ => Json.Null)

  implicit def valueSpecificationEncoder[A: JsonEncoder]: JsonEncoder[ValueSpecification[A]] =
    Json.encoder.contramap[ValueSpecification[A]] { specification =>
      Json.Obj(
        "inputs"  -> toJsonAstOrThrow(specification.inputs),
        "outputs" -> toJsonAstOrThrow(specification.output)
      )
    }

  //   sealed case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]) extends Value[TA, VA]
  implicit def ApplyValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Apply[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Value[TA, VA]].contramap {
      case Value.Apply(attributes, function, argument) =>
        ("apply", attributes, function, argument)
    }

  //   sealed case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
  implicit def ConstructorValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Constructor[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case Value.Constructor(attributes, name) =>
      ("constructor", attributes, name)
    }

  //   sealed case class Destructure[+TA, +VA](attributes: VA, pattern: Pattern[VA], valueToDestruct: Value[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def DestructureValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Destructure[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Pattern[VA], Value[TA, VA], Value[TA, VA]].contramap {
      case Value.Destructure(attributes, pattern, valueToDestruct, inValue) =>
        ("destructure", attributes, pattern, valueToDestruct, inValue)
    }

  //   sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
  implicit def FieldValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Field[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Name].contramap {
      case Value.Field(attributes, subjectValue, fieldName) =>
        ("field", attributes, subjectValue, fieldName)
    }

  //   sealed case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def FieldFunctionValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.FieldFunction[VA]] =
    JsonEncoder.tuple3[String, VA, Name].contramap { case Value.FieldFunction(attributes, name) =>
      ("field_function", attributes, name)
    }

  //   sealed case class IfThenElse[+TA, +VA](attributes: VA, condition: Value[TA, VA], thenBranch: Value[TA, VA], elseBranch: Value[TA, VA]) extends Value[TA, VA]
  implicit def IfThenElseValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.IfThenElse[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Value[TA, VA], Value[TA, VA], Value[TA, VA]].contramap {
      case Value.IfThenElse(attributes, condition, thenBranch, elseBranch) =>
        ("if_then_else", attributes, condition, thenBranch, elseBranch)
    }

  //   sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])  extends Value[TA, VA]
  implicit def LambdaValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Lambda[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Pattern[VA], Value[TA, VA]].contramap {
      case Value.Lambda(attributes, argumentPattern, body) => ("lambda", attributes, argumentPattern, body)
    }

  //   sealed case class LetDefinition[+TA, +VA](attributes: VA, valueName: Name, valueDefinition: Definition[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def LetDefinitionValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]
      : JsonEncoder[Value.LetDefinition[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Name, ValueDefinition[TA, VA], Value[TA, VA]].contramap {
      case Value.LetDefinition(attributes, valueName, valueDefinition, inValue) =>
        ("let_definition", attributes, valueName, valueDefinition, inValue)
    }

  //   sealed case class LetRecursion[+TA, +VA](attributes: VA, valueDefinitions: Map[Name, Definition[TA, VA]], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def LetRecursionValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.LetRecursion[TA, VA]] =
    JsonEncoder.tuple4[String, VA, List[(Name, ValueDefinition[TA, VA])], Value[TA, VA]].contramap {
      case Value.LetRecursion(attributes, valueDefinitions, inValue) =>
        ("let_recursion", attributes, valueDefinitions.toList, inValue)
    }

  //    sealed case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def ListValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.List[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[Value[TA, VA]]].contramap { case Value.List(attributes, elements) =>
      ("list", attributes, elements)
    }

  //   sealed case class Literal[+VA](attributes: VA, literal: Lit) extends Value[Nothing, VA]
  implicit def LiteralValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Literal[VA]] =
    JsonEncoder.tuple3[String, VA, Literal].contramap { case Value.Literal(attributes, literal) =>
      ("literal", attributes, literal)
    }

  // sealed case class PatternMatch[+TA, +VA](attributes: VA, branchOutOn: Value[TA, VA], cases: Chunk[(Pattern[VA], Value[TA, VA])]) extends Value[TA, VA]
  implicit def PatternMatchValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.PatternMatch[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Chunk[(Pattern[VA], Value[TA, VA])]].contramap {
      case Value.PatternMatch(attributes, branchOutOn, cases) =>
        ("pattern_match", attributes, branchOutOn, cases)
    }

  //   sealed case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  implicit def RecordValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Record[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[(Name, Value[TA, VA])]].contramap { case Value.Record(attributes, fields) =>
      ("record", attributes, fields)
    }

  //   sealed case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
  implicit def ReferenceValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Reference[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case Value.Reference(attributes, fullyQualifiedName) =>
      ("reference", attributes, fullyQualifiedName)
    }

  //   sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def TupleValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Tuple[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[Value[TA, VA]]].contramap { case Value.Tuple(attributes, elements) =>
      ("tuple", attributes, elements)
    }

  //   sealed case class UpdateRecord[+TA, +VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]]) extends Value[TA, VA]
  implicit def UpdateRecordValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.UpdateRecord[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], List[(Name, Value[TA, VA])]].contramap {
      case Value.UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
        ("update_record", attributes, valueToUpdate, fieldsToUpdate.toList)
    }

  implicit def UnitValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Unit[VA]] =
    JsonEncoder.tuple2[String, VA].contramap { case Value.Unit(attributes) =>
      ("unit", attributes)
    }

  //   sealed case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def VariableValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Variable[VA]] =
    JsonEncoder.tuple3[String, VA, Name].contramap { case Value.Variable(attributes, name) =>
      ("variable", attributes, name)
    }

  implicit def valueEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value[TA, VA]] =
    new JsonEncoder[Value[TA, VA]] {
      def unsafeEncode(value: Value[TA, VA], indent: Option[Int], out: Write): Unit = value match {
        case t @ Value.Apply(_, _, _) =>
          JsonEncoder[Value.Apply[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Constructor(_, _) =>
          JsonEncoder[Value.Constructor[VA]].unsafeEncode(t, indent, out)
        case t @ Value.Destructure(_, _, _, _) =>
          JsonEncoder[Value.Destructure[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Field(_, _, _) =>
          JsonEncoder[Value.Field[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.FieldFunction(_, _) =>
          JsonEncoder[Value.FieldFunction[VA]].unsafeEncode(t, indent, out)
        case t @ Value.IfThenElse(_, _, _, _) =>
          JsonEncoder[Value.IfThenElse[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Lambda(_, _, _) =>
          JsonEncoder[Value.Lambda[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.LetDefinition(_, _, _, _) =>
          JsonEncoder[Value.LetDefinition[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.LetRecursion(_, _, _) =>
          JsonEncoder[Value.LetRecursion[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.List(_, _) =>
          JsonEncoder[Value.List[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Literal(_, _) =>
          JsonEncoder[Value.Literal[VA]].unsafeEncode(t, indent, out)
        case t @ Value.PatternMatch(_, _, _) =>
          JsonEncoder[Value.PatternMatch[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Record(_, _) =>
          JsonEncoder[Value.Record[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Reference(_, _) =>
          JsonEncoder[Value.Reference[VA]].unsafeEncode(t, indent, out)
        case t @ Value.Tuple(_, _) =>
          JsonEncoder[Value.Tuple[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.UpdateRecord(_, _, _) =>
          JsonEncoder[Value.UpdateRecord[TA, VA]].unsafeEncode(t, indent, out)
        case t @ Value.Unit(_) =>
          JsonEncoder[Value.Unit[VA]].unsafeEncode(t, indent, out)
        case t @ Value.Variable(_, _) =>
          JsonEncoder[Value.Variable[VA]].unsafeEncode(t, indent, out)
      }
    }

  implicit def ExtensibleRecordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.ExtensibleRecord[A]] =
    JsonEncoder.tuple4[String, A, Name, Chunk[Field[Type[A]]]].contramap {
      case Type.ExtensibleRecord(attributes, name, fields) => ("extensible_record", attributes, name, fields)
    }

  implicit def FunctionTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Function[A]] =
    JsonEncoder.tuple4[String, A, Type[A], Type[A]].contramap {
      case Type.Function(attributes, argumentType, returnType) =>
        ("function", attributes, argumentType, returnType)
    }

  implicit def RecordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Record[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Field[Type[A]]]].contramap { case Type.Record(attributes, fields) =>
      ("record", attributes, fields)
    }

  implicit def ReferenceTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Reference[A]] =
    JsonEncoder.tuple4[String, A, FQName, Chunk[Type[A]]].contramap {
      case Type.Reference(attributes, typeName, typeParams) =>
        ("reference", attributes, typeName, typeParams)
    }

  implicit def TupleTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Tuple[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Type[A]]].contramap { case Type.Tuple(attributes, elements) =>
      ("tuple", attributes, elements)
    }

  implicit def UnitTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Unit[A]] =
    JsonEncoder.tuple2[String, A].contramap[Type.Unit[A]] { case Type.Unit(attributes) =>
      ("unit", attributes)
    }

  implicit def VariableTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Variable[A]] =
    JsonEncoder.tuple3[String, A, Name].contramap[Type.Variable[A]] { case Type.Variable(attributes, name) =>
      ("variable", attributes, name)
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

  implicit def documentedEncoder[A: JsonEncoder]: JsonEncoder[Documented[A]] =
    Json.encoder.contramap[Documented[A]] { documented =>
      Json.Arr(Json.Str(documented.doc), toJsonAstOrThrow(documented.value))
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
          Json.Obj(
            "name" -> toJsonAstOrThrow(name),
            "spec" -> toJsonAstOrThrow(moduleSpecification)
          )
        })
      )
    }

  implicit def packageDefinitionEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[PackageDefinition[TA, VA]] =
    Json.encoder.contramap[PackageDefinition[TA, VA]] { definition =>
      Json.Obj(
        "modules" -> toJsonAstOrThrow(definition.modules.toList.map { case (name, moduleSpecification) =>
          Json.Obj(
            "name" -> toJsonAstOrThrow(name),
            "def"  -> toJsonAstOrThrow(moduleSpecification)
          )
        })
      )
    }

  private def toJsonAstOrThrow[A](a: A)(implicit encoder: JsonEncoder[A]): Json =
    a.toJsonAST.toOption.get
}

object MorphirJsonEncodingSupportV1 extends MorphirJsonEncodingSupportV1

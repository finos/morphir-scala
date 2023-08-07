package org.finos.morphir.ir
package json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type.{Constructors, Definition => TypeDefinition, Specification => TypeSpecification, Type}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir.Value.{Value, _}
import org.finos.morphir.ir.module.{Definition => ModuleDefinition, Specification => ModuleSpecification}

trait MorphirJsonEncodingSupportV1 extends JsonEncodingHelpers {
  implicit val unitEncoder: JsonEncoder[Unit] = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val pathEncoder: JsonEncoder[Path] = JsonEncoder.list[Name].contramap(path => path.segments.toList)
  implicit val moduleNameEncoder: JsonEncoder[ModuleName]   = pathEncoder.contramap(_.toPath)
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

  // implicit val qualifiedModuleNameEncoder: JsonEncoder[QualifiedModuleName] =
  //   Json.encoder.contramap[QualifiedModuleName](moduleName =>
  //     Json.Arr(toJsonAstOrThrow(moduleName.namespace), toJsonAstOrThrow(moduleName.localName))
  //   )

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

  implicit def patternAsPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.AsPattern[A]] =
    JsonEncoder.tuple4[String, A, Pattern[A], Name].contramap { case Pattern.AsPattern(attributes, pattern, name) =>
      ("as_pattern", attributes, pattern, name)
    }

  implicit def patternConstructorPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.ConstructorPattern[A]] =
    JsonEncoder.tuple4[String, A, FQName, Chunk[Pattern[A]]].contramap {
      case Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns) =>
        ("constructor_pattern", attributes, constructorName, argumentPatterns)
    }

  implicit def patternEmptyListPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.EmptyListPattern[A]] =
    JsonEncoder.tuple2[String, A].contramap { case Pattern.EmptyListPattern(attributes) =>
      ("empty_list_pattern", attributes)
    }

  implicit def patternHeadTailPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.HeadTailPattern[A]] =
    JsonEncoder.tuple4[String, A, Pattern[A], Pattern[A]].contramap {
      case Pattern.HeadTailPattern(attributes, headPattern, tailPattern) =>
        ("head_tail_pattern", attributes, headPattern, tailPattern)
    }

  implicit def patternLiteralPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.LiteralPattern[A]] =
    JsonEncoder.tuple3[String, A, Literal].contramap { case Pattern.LiteralPattern(attributes, literal) =>
      ("literal_pattern", attributes, literal)
    }

  implicit def patternTuplePatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.TuplePattern[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Pattern[A]]].contramap {
      case Pattern.TuplePattern(attributes, elementPatterns) =>
        ("tuple_pattern", attributes, elementPatterns)
    }

  implicit def patternUnitPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.UnitPattern[A]] =
    JsonEncoder.tuple2[String, A].contramap { case Pattern.UnitPattern(attributes) =>
      ("unit_pattern", attributes)
    }

  implicit def patternWildcardPatternEncoder[A: JsonEncoder]: JsonEncoder[Pattern.WildcardPattern[A]] =
    JsonEncoder.tuple2[String, A].contramap { case Pattern.WildcardPattern(attributes) =>
      ("wildcard_pattern", attributes)
    }

  implicit def patternEncoder[A: JsonEncoder]: JsonEncoder[Pattern[A]] =
    new JsonEncoder[Pattern[A]] {
      def unsafeEncode(pattern: Pattern[A], indent: Option[Int], out: Write): Unit = pattern match {
        case pattern @ Pattern.AsPattern(_, _, _) =>
          JsonEncoder[Pattern.AsPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.ConstructorPattern(_, _, _) =>
          JsonEncoder[Pattern.ConstructorPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.EmptyListPattern(_) =>
          JsonEncoder[Pattern.EmptyListPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.HeadTailPattern(_, _, _) =>
          JsonEncoder[Pattern.HeadTailPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.LiteralPattern(_, _) =>
          JsonEncoder[Pattern.LiteralPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.TuplePattern(_, _) =>
          JsonEncoder[Pattern.TuplePattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.UnitPattern(_) =>
          JsonEncoder[Pattern.UnitPattern[A]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.WildcardPattern(_) =>
          JsonEncoder[Pattern.WildcardPattern[A]].unsafeEncode(pattern, indent, out)
      }
    }

  implicit def constructorsEncoder[A: JsonEncoder]: JsonEncoder[Constructors[A]] =
    Json.encoder.contramap[Constructors[A]] { ctors =>
      toJsonAstOrThrow(
        ctors.toMap.toList.map { case (ctorName: Name, ctorArgs: Chunk[(Name, Type[A])]) =>
          (
            toJsonAstOrThrow(ctorName),
            toJsonAstOrThrow(
              ctorArgs.map { case (argName: Name, argType: Type[A]) =>
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
        case AccessControlled.Access.Public  => Json.Arr(Json.Str("public"), toJsonAstOrThrow(accessControlled.value))
        case AccessControlled.Access.Private => Json.Arr(Json.Str("private"), toJsonAstOrThrow(accessControlled.value))
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

  implicit def typeDefinitionEncoder[A: JsonEncoder]: JsonEncoder[TypeDefinition[A]] =
    new JsonEncoder[TypeDefinition[A]] {
      def unsafeEncode(d: TypeDefinition[A], indent: Option[Int], out: Write): Unit = d match {
        case d @ TypeDefinition.TypeAlias(_, _) =>
          JsonEncoder[TypeDefinition.TypeAlias[A]].unsafeEncode(d, indent, out)
        case d @ TypeDefinition.CustomType(_, _) =>
          JsonEncoder[TypeDefinition.CustomType[A]].unsafeEncode(d, indent, out)
      }
    }

  implicit def typeSpecificationTypeAliasEncoder[A: JsonEncoder]
      : JsonEncoder[TypeSpecification.TypeAliasSpecification[A]] =
    Json.encoder.contramap[TypeSpecification.TypeAliasSpecification[A]] { alias =>
      Json.Arr(Json.Str("type_alias_specification"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.expr))
    }

  implicit def typeSpecificationEncoderCustomTypeEncoder[A: JsonEncoder]
      : JsonEncoder[TypeSpecification.CustomTypeSpecification[A]] =
    Json.encoder.contramap[TypeSpecification.CustomTypeSpecification[A]] { tpe =>
      Json.Arr(Json.Str("custom_type_specification"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeSpecificationEncoderOpaqueTypeEncoder2: JsonEncoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonEncoder.tuple2[String, Chunk[Name]].contramap {
      case TypeSpecification.OpaqueTypeSpecification(typeParams: Chunk[Name]) =>
        ("opaque_type_specification", typeParams)
    }

  implicit def typeSpecificationEncoder[A: JsonEncoder]: JsonEncoder[TypeSpecification[A]] =
    new JsonEncoder[TypeSpecification[A]] {
      def unsafeEncode(spec: TypeSpecification[A], indent: Option[Int], out: Write): Unit =
        spec match {
          case spec @ TypeSpecification.TypeAliasSpecification(_, _) =>
            JsonEncoder[TypeSpecification.TypeAliasSpecification[A]].unsafeEncode(spec, indent, out)
          case spec @ TypeSpecification.CustomTypeSpecification(_, _) =>
            JsonEncoder[TypeSpecification.CustomTypeSpecification[A]].unsafeEncode(spec, indent, out)
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
  implicit def applyValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Apply[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Value[TA, VA]].contramap {
      case Value.Apply(attributes, function, argument) =>
        ("apply", attributes, function, argument)
    }

  //   sealed case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
  implicit def constructorValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Constructor[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case Value.Constructor(attributes, name) =>
      ("constructor", attributes, name)
    }

  //   sealed case class Destructure[+TA, +VA](attributes: VA, pattern: Pattern[VA], valueToDestruct: Value[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def destructureValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Destructure[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Pattern[VA], Value[TA, VA], Value[TA, VA]].contramap {
      case Value.Destructure(attributes, pattern, valueToDestruct, inValue) =>
        ("destructure", attributes, pattern, valueToDestruct, inValue)
    }

  //   sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
  implicit def fieldValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Field[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Name].contramap {
      case Value.Field(attributes, subjectValue, fieldName) =>
        ("field", attributes, subjectValue, fieldName)
    }

  //   sealed case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def fieldFunctionValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.FieldFunction[VA]] =
    JsonEncoder.tuple3[String, VA, Name].contramap { case Value.FieldFunction(attributes, name) =>
      ("field_function", attributes, name)
    }

  //   sealed case class IfThenElse[+TA, +VA](attributes: VA, condition: Value[TA, VA], thenBranch: Value[TA, VA], elseBranch: Value[TA, VA]) extends Value[TA, VA]
  implicit def ifThenElseValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.IfThenElse[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Value[TA, VA], Value[TA, VA], Value[TA, VA]].contramap {
      case Value.IfThenElse(attributes, condition, thenBranch, elseBranch) =>
        ("if_then_else", attributes, condition, thenBranch, elseBranch)
    }

  //   sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])  extends Value[TA, VA]
  implicit def lambdaValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Lambda[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Pattern[VA], Value[TA, VA]].contramap {
      case Value.Lambda(attributes, argumentPattern, body) => ("lambda", attributes, argumentPattern, body)
    }

  //   sealed case class LetDefinition[+TA, +VA](attributes: VA, valueName: Name, valueDefinition: Definition[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letDefinitionValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]
      : JsonEncoder[Value.LetDefinition[TA, VA]] =
    JsonEncoder.tuple5[String, VA, Name, ValueDefinition[TA, VA], Value[TA, VA]].contramap {
      case Value.LetDefinition(attributes, valueName, valueDefinition, inValue) =>
        ("let_definition", attributes, valueName, valueDefinition, inValue)
    }

  //   sealed case class LetRecursion[+TA, +VA](attributes: VA, valueDefinitions: Map[Name, Definition[TA, VA]], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letRecursionValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.LetRecursion[TA, VA]] =
    JsonEncoder.tuple4[String, VA, List[(Name, ValueDefinition[TA, VA])], Value[TA, VA]].contramap {
      case Value.LetRecursion(attributes, valueDefinitions, inValue) =>
        ("let_recursion", attributes, valueDefinitions.toList, inValue)
    }

  //    sealed case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def listValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.List[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[Value[TA, VA]]].contramap { case Value.List(attributes, elements) =>
      ("list", attributes, elements)
    }

  //   sealed case class Literal[+VA](attributes: VA, literal: Lit) extends Value[Nothing, VA]
  implicit def literalValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Literal[VA]] =
    JsonEncoder.tuple3[String, VA, Literal].contramap { case Value.Literal(attributes, literal) =>
      ("literal", attributes, literal)
    }

  // sealed case class PatternMatch[+TA, +VA](attributes: VA, branchOutOn: Value[TA, VA], cases: Chunk[(Pattern[VA], Value[TA, VA])]) extends Value[TA, VA]
  implicit def patternMatchValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.PatternMatch[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], Chunk[(Pattern[VA], Value[TA, VA])]].contramap {
      case Value.PatternMatch(attributes, branchOutOn, cases) =>
        ("pattern_match", attributes, branchOutOn, cases)
    }

  //   sealed case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  implicit def recordValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Record[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[(Name, Value[TA, VA])]].contramap { case Value.Record(attributes, fields) =>
      ("record", attributes, fields)
    }

  //   sealed case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
  implicit def referenceValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Reference[VA]] =
    JsonEncoder.tuple3[String, VA, FQName].contramap { case Value.Reference(attributes, fullyQualifiedName) =>
      ("reference", attributes, fullyQualifiedName)
    }

  //   sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def tupleValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.Tuple[TA, VA]] =
    JsonEncoder.tuple3[String, VA, Chunk[Value[TA, VA]]].contramap { case Value.Tuple(attributes, elements) =>
      ("tuple", attributes, elements)
    }

  //   sealed case class UpdateRecord[+TA, +VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]]) extends Value[TA, VA]
  implicit def updateRecordValueJsonEncoder[TA: JsonEncoder, VA: JsonEncoder]: JsonEncoder[Value.UpdateRecord[TA, VA]] =
    JsonEncoder.tuple4[String, VA, Value[TA, VA], List[(Name, Value[TA, VA])]].contramap {
      case Value.UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
        ("update_record", attributes, valueToUpdate, fieldsToUpdate.toList)
    }

  implicit def unitValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Unit[VA]] =
    JsonEncoder.tuple2[String, VA].contramap { case Value.Unit(attributes) =>
      ("unit", attributes)
    }

  //   sealed case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def variableValueJsonEncoder[VA: JsonEncoder]: JsonEncoder[Value.Variable[VA]] =
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

  implicit def extensibleRecordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.ExtensibleRecord[A]] =
    JsonEncoder.tuple4[String, A, Name, Chunk[Field[Type[A]]]].contramap {
      case Type.ExtensibleRecord(attributes, name, fields) => ("extensible_record", attributes, name, fields)
    }

  implicit def functionTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Function[A]] =
    JsonEncoder.tuple4[String, A, Type[A], Type[A]].contramap {
      case Type.Function(attributes, argumentType, returnType) =>
        ("function", attributes, argumentType, returnType)
    }

  implicit def recordTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Record[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Field[Type[A]]]].contramap { case Type.Record(attributes, fields) =>
      ("record", attributes, fields)
    }

  implicit def referenceTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Reference[A]] =
    JsonEncoder.tuple4[String, A, FQName, Chunk[Type[A]]].contramap {
      case Type.Reference(attributes, typeName, typeParams) =>
        ("reference", attributes, typeName, typeParams)
    }

  implicit def tupleTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Tuple[A]] =
    JsonEncoder.tuple3[String, A, Chunk[Type[A]]].contramap { case Type.Tuple(attributes, elements) =>
      ("tuple", attributes, elements)
    }

  implicit def unitTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Unit[A]] =
    JsonEncoder.tuple2[String, A].contramap[Type.Unit[A]] { case Type.Unit(attributes) =>
      ("unit", attributes)
    }

  implicit def variableTypeJsonEncoder[A: JsonEncoder]: JsonEncoder[Type.Variable[A]] =
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

  implicit def distributionLibraryJsonEncoder: JsonEncoder[Library] =
    JsonEncoder
      .tuple4[String, PackageName, List[(PackageName, UPackageSpecification)], PackageDefinition.Typed]
      .contramap { case Library(packageName, dependencies, packageDef) =>
        ("library", packageName, dependencies.toList, packageDef)
      }

  implicit def distributionEncoder: JsonEncoder[Distribution] =
    new JsonEncoder[Distribution] {
      def unsafeEncode(a: Distribution, indent: Option[Int], out: Write): Unit = a match {
        case library: Library => distributionLibraryJsonEncoder.unsafeEncode(library, indent, out)
      }
    }

  implicit val morphirIRVersionEncoder: JsonEncoder[MorphirIRVersion] =
    JsonEncoder.int.contramap(_.versionNumber.toDouble.toInt)

  implicit def morphirIRFileJsonEncoder: JsonEncoder[MorphirIRFile] =
    Json.encoder.contramap[MorphirIRFile] { file =>
      Json.Obj(
        "formatVersion" -> toJsonAstOrThrow(file.version),
        "distribution"  -> toJsonAstOrThrow(file.distribution)
      )
    }
}

object MorphirJsonEncodingSupportV1 extends MorphirJsonEncodingSupportV1

package org.finos.morphir.ir
package json

import zio._
import zio.json._
import zio.json.ast.Json

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
import org.finos.morphir.ir.value.recursive.ValueCase
import org.finos.morphir.ir.{Literal, _}

import scala.annotation.nowarn

trait MorphirJsonDecodingSupport {
  implicit val unitDecoder: JsonDecoder[Unit] = JsonDecoder.list[String].mapOrFail {
    case a if a.isEmpty => Right(())
    case a              => Left(s"Expected empty list, got [${a.mkString(", ")}]")
  }
  implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
  implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
  implicit val modulePathDecoder: JsonDecoder[ModulePath]   = pathDecoder.map(ModulePath(_))
  implicit val packageNameDecoder: JsonDecoder[PackageName] = pathDecoder.map(PackageName(_))
  implicit val qNameDecoder: JsonDecoder[QName]             = JsonDecoder.tuple2[Path, Name].map(QName.fromTuple)
  implicit val fqNameDecoder: JsonDecoder[FQName] = JsonDecoder.tuple3[PackageName, ModulePath, Name].map {
    case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
  }

  implicit val moduleNameDecoder: JsonDecoder[ModuleName] =
    JsonDecoder.tuple2[Path, Name].map { case (namespace, localName) =>
      ModuleName(namespace, localName)
    }

  implicit def literalBoolDecoder: JsonDecoder[Literal.Bool] =
    JsonDecoder.tuple2[String, Boolean].mapOrFail {
      case ("BoolLiteral", value) => Right(Literal.Bool(value))
      case (other, value)         => Left(s"Expected BoolLiteral, got $other with value $value")
    }

  implicit def literalCharDecoder: JsonDecoder[Literal.Char] =
    JsonDecoder.tuple2[String, Char].mapOrFail {
      case ("CharLiteral", value) => Right(Literal.Char(value))
      case (other, value)         => Left(s"Expected CharLiteral, got $other with value $value")
    }

  implicit def literalFloatDecoder: JsonDecoder[Literal.Float] =
    JsonDecoder.tuple2[String, java.math.BigDecimal].mapOrFail {
      case ("FloatLiteral", value) => Right(Literal.Float(value))
      case (other, value)          => Left(s"Expected FloatLiteral, got $other with value $value")
    }

  implicit def literalStringDecoder: JsonDecoder[Literal.String] =
    JsonDecoder.tuple2[String, String].mapOrFail {
      case ("StringLiteral", value) => Right(Literal.String(value))
      case (other, value)           => Left(s"Expected StringLiteral, got $other with value $value")
    }

  implicit def literalWholeNumberDecoder: JsonDecoder[Literal.WholeNumber] =
    JsonDecoder.tuple2[String, java.math.BigInteger].mapOrFail {
      case ("WholeNumberLiteral", value) => Right(Literal.WholeNumber(value))
      case (other, value)                => Left(s"Expected WholeNumberLiteral, got $other with value $value")
    }

  implicit def literalDecoder: JsonDecoder[Literal[Any]] =
    literalBoolDecoder.widen[Literal[Any]] orElse
      literalCharDecoder.widen[Literal[Any]] orElse
      literalFloatDecoder.widen[Literal[Any]] orElse
      literalStringDecoder.widen[Literal[Any]] orElse
      literalWholeNumberDecoder.widen[Literal[Any]]

  implicit def fieldDecoder[A: JsonDecoder]: JsonDecoder[Field[A]] = {
    final case class FieldLike[A](name: Name, tpe: A)
    lazy val dec: JsonDecoder[FieldLike[A]] = DeriveJsonDecoder.gen
    dec.map(f => Field(f.name, f.tpe))
  }

  implicit def documentedDecoder[A: JsonDecoder]: JsonDecoder[Documented[A]] = {
    lazy val decoder: JsonDecoder[Documented[A]] = DeriveJsonDecoder.gen
    decoder
  }

  implicit def accessDecoder: JsonDecoder[AccessControlled.Access] =
    JsonDecoder.string.map { access =>
      access match {
        case "Public"  => AccessControlled.Access.Public
        case "Private" => AccessControlled.Access.Private
      }
    }

  implicit def accessControlledDecoder[A: JsonDecoder]: JsonDecoder[AccessControlled[A]] = {
    lazy val dec: JsonDecoder[AccessControlled[A]] = DeriveJsonDecoder.gen
    dec
  }

  implicit def extensibleRecordCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.ExtensibleRecord[A]] =
    JsonDecoder.tuple4[String, A, Name, Chunk[Field[Type[A]]]].mapOrFail {
      case ("ExtensibleRecord", attributes, name, fields) =>
        Right(Type.ExtensibleRecord(attributes, name, fields))
      case (other, attributes, name, fields) =>
        Left(s"Expected ExtensibleRecord, got $other with attributes: $attributes, name: $name and fields: $fields")
    }

  implicit def functionCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Function[A]] =
    JsonDecoder.tuple4[String, A, Type[A], Type[A]].mapOrFail {
      case ("Function", attributes, argumentType, returnType) =>
        Right(Type.Function(attributes, argumentType, returnType))
      case (other, attributes, argumentType, returnType) =>
        Left(
          s"Expected Function, got $other with attributes: $attributes, argumentType: $argumentType and returnType: $returnType"
        )
    }

  implicit def recordCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Record[A]] =
    JsonDecoder.tuple3[String, A, Chunk[Field[Type[A]]]].mapOrFail {
      case ("Record", attributes, fields) =>
        Right(Type.Record(attributes, fields))
      case (other, attributes, fields) =>
        Left(
          s"Expected Record, got $other with attributes: $attributes and fields: $fields"
        )
    }

  implicit def referenceCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Reference[A]] =
    JsonDecoder.tuple4[String, A, FQName, Chunk[Type[A]]].mapOrFail {
      case ("Reference", attributes, typeName, typeParams) =>
        Right(Type.Reference(attributes, typeName, typeParams))
      case (other, attributes, typeName, typeParams) =>
        Left(
          s"Expected Reference, got $other with attributes: $attributes, typeName: $typeName and typeParams: $typeParams"
        )
    }

  implicit def tupleCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Tuple[A]] =
    JsonDecoder.tuple3[String, A, Chunk[Type[A]]].mapOrFail {
      case ("Tuple", attributes, elements) =>
        Right(Type.Tuple(attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected Tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  implicit def unitCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Unit[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("Unit", attributes) =>
        Right(Type.Unit(attributes))
      case (other, attributes) =>
        Left(
          s"Expected Unit, got $other with attributes: $attributes"
        )
    }

  implicit def variableCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Variable[A]] =
    JsonDecoder.tuple3[String, A, Name].mapOrFail {
      case ("Variable", attributes, name) =>
        Right(Type.Variable(attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected Variable, got $other with attributes: $attributes and name: $name"
        )
    }

  @nowarn("msg=Implicit resolves to enclosing method typeDecoder")
  implicit def typeDecoder[A: JsonDecoder]: JsonDecoder[Type[A]] =
    unitCaseTypeDecoder[A].widen[Type[A]] orElse
      variableCaseTypeDecoder[A].widen[Type[A]] orElse
      tupleCaseTypeDecoder[A].widen[Type[A]] orElse
      recordCaseTypeDecoder[A].widen[Type[A]] orElse
      extensibleRecordCaseTypeDecoder[A].widen[Type[A]] orElse
      functionCaseTypeDecoder[A].widen[Type[A]] orElse
      referenceCaseTypeDecoder[A].widen[Type[A]]

  implicit def constructorDecoder[A: JsonDecoder]: JsonDecoder[Constructors[A]] =
    JsonDecoder.list[(Name, Chunk[(Name, Type[A])])].map {
      case Nil          => Constructors(Map.empty)
      case constructors => Constructors(constructors.toMap)
    }

  implicit def typeDefinitionTypeAliasDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition.TypeAlias[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Type[A]].mapOrFail {
      case ("TypeAliasDefinition", typeParams, typeExp) =>
        Right(TypeDefinition.TypeAlias(typeParams, typeExp))
      case (other, typeParams, typeExp) =>
        Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and typeExp: $typeExp")
    }

  implicit def typeDefinitionCustomTypeDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition.CustomType[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], AccessControlled[Constructors[A]]].mapOrFail {
      case ("CustomTypeDefinition", typeParams, ctors) =>
        Right(TypeDefinition.CustomType(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and ctors: $ctors")
    }

  implicit def typeDefinitionDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition[A]] =
    typeDefinitionTypeAliasDecoder[A].widen[TypeDefinition[A]] orElse
      typeDefinitionCustomTypeDecoder[A].widen[TypeDefinition[A]]

  implicit def typeSpecificationTypeAliasDecoder[A: JsonDecoder]
      : JsonDecoder[TypeSpecification.TypeAliasSpecification[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Type[A]].mapOrFail {
      case ("TypeAliasSpecification", typeParams, expr) =>
        Right(TypeSpecification.TypeAliasSpecification(typeParams, expr))
      case (other, typeParams, expr) =>
        Left(s"Expected type_alias_specification, got $other with typeParams: $typeParams and expr: $expr")
    }

  implicit def typeSpecificationOpaqueTypeDecoder: JsonDecoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonDecoder.tuple2[String, Chunk[Name]].mapOrFail {
      case ("OpaqueTypeSpecification", typeParams) =>
        Right(TypeSpecification.OpaqueTypeSpecification(typeParams))
      case (other, typeParams) =>
        Left(s"Expected opaque_type_specification, got $other with typeParams: $typeParams")
    }

  implicit def typeSpecificationCustomTypeDecoder[A: JsonDecoder]
      : JsonDecoder[TypeSpecification.CustomTypeSpecification[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Constructors[A]].mapOrFail {
      case ("CustomTypeSpecification", typeParams, ctors) =>
        Right(TypeSpecification.CustomTypeSpecification(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected custom_type_specification, got $other with typeParams: $typeParams and ctors: $ctors")
    }

  implicit def typeSpecificationDecoder[A: JsonDecoder]: JsonDecoder[TypeSpecification[A]] =
    typeSpecificationTypeAliasDecoder[A].widen[TypeSpecification[A]] orElse
      typeSpecificationCustomTypeDecoder[A].widen[TypeSpecification[A]] orElse
      typeSpecificationOpaqueTypeDecoder.widen[TypeSpecification[A]]

  implicit def valueDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[ValueDefinition[TA, VA]] = {
    lazy val dec: JsonDecoder[ValueDefinition[TA, VA]] = DeriveJsonDecoder.gen
    dec
  }

  // final case class Case[+TA, +VA, +TypeRepr[+_]: Covariant, +Z](
  //   inputTypes: Chunk[(Name, VA, TypeRepr[TA])],
  //   outputType: TypeRepr[TA],
  //   body: Z
  // )
  implicit def valueDefinitionCaseDecoder[TA: JsonDecoder, VA: JsonDecoder, Z: JsonDecoder]
      : JsonDecoder[ValueDefinition.Case[TA, VA, Type, Z]] =
    Json.Obj.decoder.mapOrFail[ValueDefinition.Case[TA, VA, Type, Z]] {
      case Json.Obj(
            Chunk(
              "inputTypes" -> inputTypes,
              "outputType" -> outputType,
              "body" -> body
            )
          ) =>
        for {
          a <- JsonDecoder[Chunk[(Name, VA, Type[TA])]].fromJsonAST(inputTypes)
          b <- JsonDecoder[Type[TA]].fromJsonAST(outputType)
          c <- JsonDecoder[Z].fromJsonAST(body)
        } yield ValueDefinition.Case(a, b, c)
      case other =>
        Left(
          s"Cannot decode: $other"
        )
    }

  implicit def anyDecoder: JsonDecoder[Any] =
    Json.Null.decoder.map(v => ())

  implicit def valueSpecificationDecoder[A: JsonDecoder]: JsonDecoder[ValueSpecification[A]] = {
    final case class Spec[A](inputs: Chunk[(Name, Type[A])], outputs: Type[A])
    lazy val dec: JsonDecoder[Spec[A]] = DeriveJsonDecoder.gen
    dec.map(spec => ValueSpecification(spec.inputs, spec.outputs))
  }

  implicit def patternAsPatternDecoder[Attributes: JsonDecoder]: JsonDecoder[Pattern.AsPattern[Attributes]] =
    JsonDecoder.tuple4[String, Attributes, Pattern[Attributes], Name].mapOrFail {
      case ("as_pattern", attributes, pattern, name) => Right(Pattern.AsPattern(pattern, name, attributes))
      case (other, attributes, pattern, name) =>
        Left(
          s"Expected as_pattern, got $other with attributes: $attributes, pattern: $pattern and name: $name"
        )
    }

  implicit def patternConstructorPatternDecoder[Attributes: JsonDecoder]
      : JsonDecoder[Pattern.ConstructorPattern[Attributes]] =
    JsonDecoder.tuple4[String, Attributes, FQName, Chunk[Pattern[Attributes]]].mapOrFail {
      case ("constructor_pattern", attributes, constructorName, argumentPatterns) =>
        Right(Pattern.ConstructorPattern(constructorName, argumentPatterns, attributes))
      case (other, attributes, constructorName, argumentPatterns) =>
        Left(
          s"Expected constructor_pattern, got $other with attributes: $attributes, constructorName: $constructorName and argumentPatterns: $argumentPatterns"
        )
    }

  implicit def patternEmptyListPatternDecoder[Attributes: JsonDecoder]
      : JsonDecoder[Pattern.EmptyListPattern[Attributes]] =
    JsonDecoder.tuple2[String, Attributes].mapOrFail {
      case ("empty_list_pattern", attributes) =>
        Right(Pattern.EmptyListPattern[Attributes](attributes))
      case (other, attributes) =>
        Left(s"Expected empty_list_pattern, got $other with attributes: $attributes")
    }

  implicit def patternHeadTailPatternDecoder[Attributes: JsonDecoder]
      : JsonDecoder[Pattern.HeadTailPattern[Attributes]] =
    JsonDecoder.tuple4[String, Attributes, Pattern[Attributes], Pattern[Attributes]].mapOrFail {
      case ("head_tail_pattern", attributes, headPattern, tailPattern) =>
        Right(Pattern.HeadTailPattern(headPattern, tailPattern, attributes))
      case (other, attributes, headPattern, tailPattern) =>
        Left(
          s"Expected head_tail_pattern, got $other with attributes: $attributes, headPattern: $headPattern and tailPattern: $tailPattern"
        )
    }

  implicit def patternLiteralPatternDecoder[Attributes: JsonDecoder]
      : JsonDecoder[Pattern.LiteralPattern[Any, Attributes]] =
    JsonDecoder.tuple3[String, Attributes, Literal[Any]].mapOrFail {
      case ("literal_pattern", attributes, literal) =>
        Right(Pattern.LiteralPattern(literal, attributes))
      case (other, attributes, literal) =>
        Left(s"Expected literal_pattern, got $other with attributes: $attributes and literal: $literal")
    }

  implicit def patternTuplePatternDecoder[Attributes: JsonDecoder]: JsonDecoder[Pattern.TuplePattern[Attributes]] =
    JsonDecoder.tuple3[String, Attributes, Chunk[Pattern[Attributes]]].mapOrFail {
      case ("tuple_pattern", attributes, elementPatterns) =>
        Right(Pattern.TuplePattern(elementPatterns, attributes))
      case (other, attributes, elementPatterns) =>
        Left(s"Expected tuple_pattern, got $other with attributes: $attributes and elementPatterns: $elementPatterns")
    }

  implicit def patternUnitPatternDecoder[Attributes: JsonDecoder]: JsonDecoder[Pattern.UnitPattern[Attributes]] =
    JsonDecoder.tuple2[String, Attributes].mapOrFail {
      case ("unit_pattern", attributes) =>
        Right(Pattern.UnitPattern[Attributes](attributes))
      case (other, attributes) =>
        Left(s"Expected unit_pattern, got $other with attributes: $attributes")
    }

  implicit def patternWildcardPatternDecoder[Attributes: JsonDecoder]
      : JsonDecoder[Pattern.WildcardPattern[Attributes]] =
    JsonDecoder.tuple2[String, Attributes].mapOrFail {
      case ("wildcard_pattern", attributes) =>
        Right(Pattern.WildcardPattern[Attributes](attributes))
      case (other, attributes) =>
        Left(s"Expected wildcard_pattern, got $other with attributes: $attributes")
    }

  implicit def patternDecoder[Attributes: JsonDecoder]: JsonDecoder[Pattern[Attributes]] =
    patternEmptyListPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternWildcardPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternUnitPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternLiteralPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternTuplePatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternHeadTailPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternConstructorPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
      patternAsPatternDecoder[Attributes].widen[Pattern[Attributes]]

  implicit def moduleSpecificationDecoder[TA](implicit
      decoder: JsonDecoder[TA]
  ): JsonDecoder[ModuleSpecification[TA]] = {
    final case class Spec[TA](
        types: List[(Name, Documented[TypeSpecification[TA]])],
        values: List[(Name, Documented[ValueSpecification[TA]])]
    )
    lazy val dec: JsonDecoder[Spec[TA]] = DeriveJsonDecoder.gen
    dec.map(spec => ModuleSpecification(spec.types.toMap, spec.values.toMap))
  }

  implicit def moduleDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[ModuleDefinition[TA, VA]] = {
    final case class Def[TA, VA](
        types: List[(Name, AccessControlled[Documented[TypeDefinition[TA]]])],
        values: List[(Name, AccessControlled[Documented[ValueDefinition[TA, VA]]])]
    )
    lazy val dec1: JsonDecoder[Def[TA, VA]] = DeriveJsonDecoder.gen
    dec1.map(d => ModuleDefinition(d.types.toMap, d.values.toMap))
  }

  // final case class Specification[+TA](modules: Map[ModuleName, ModuleSpec[TA]]) {
  implicit def packageModuleSpecificationDecoder[TA: JsonDecoder]: JsonDecoder[PackageSpecification[TA]] = {
    final case class Spec[TA](modules: List[(ModuleName, ModuleSpecification[TA])])
    lazy val dec: JsonDecoder[Spec[TA]] = DeriveJsonDecoder.gen
    dec.map(s => PackageSpecification(s.modules.map(m => m._1 -> m._2).toMap))
  }

  implicit def packageModuleDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]
      : JsonDecoder[PackageDefinition[TA, VA]] = {
    final case class Spec[TA, VA](modules: List[(ModuleName, AccessControlled[ModuleDefinition[TA, VA]])])
    lazy val dec: JsonDecoder[Spec[TA, VA]] = DeriveJsonDecoder.gen
    dec.map(d => PackageDefinition(d.modules.map(m => m._1 -> m._2).toMap))
  }

  //   final case class ApplyCase[+VA, +Self](attributes: VA, function: Self, argument: Self) extends ValueCase[Nothing, VA, Self]
  implicit def ApplyCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.ApplyCase[VA, Self]] =
    JsonDecoder.tuple4[String, VA, Self, Self].mapOrFail {
      case ("apply", attributes, function, argument) =>
        Right(ValueCase.ApplyCase[VA, Self](attributes, function, argument))
      case (other, attributes, function, argument) =>
        Left(
          s"Expected apply, got $other with attributes: $attributes, function: $function and argument: $argument"
        )
    }

  //   final case class ConstructorCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]
  implicit def ConstructorCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.ConstructorCase[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("constructor", attributes, name) =>
        Right(ValueCase.ConstructorCase[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected constructor, got $other with attributes: $attributes and name: $name"
        )
    }

  //   final case class DestructureCase[+VA, +Self](attributes: VA, pattern: Pattern[VA], valueToDestruct: Self, inValue: Self) extends ValueCase[Nothing, VA, Self]
  implicit def DestructureCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.DestructureCase[VA, Self]] =
    JsonDecoder.tuple5[String, VA, Pattern[VA], Self, Self].mapOrFail {
      case ("destructure", attributes, pattern, valueToDestruct, inValue) =>
        Right(ValueCase.DestructureCase[VA, Self](attributes, pattern, valueToDestruct, inValue))
      case (other, attributes, pattern, valueToDestruct, inValue) =>
        Left(
          s"Expected destructure, got $other with attributes: $attributes, pattern: $pattern, valueToDestruct: $valueToDestruct and inValue: $inValue"
        )
    }

  //   final case class FieldCase[+VA, +Self](attributes: VA, target: Self, name: Name) extends ValueCase[Nothing, VA, Self]
  implicit def FieldCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.FieldCase[VA, Self]] =
    JsonDecoder.tuple4[String, VA, Self, Name].mapOrFail {
      case ("field", attributes, target, name) =>
        Right(ValueCase.FieldCase[VA, Self](attributes, target, name))
      case (other, attributes, target, name) =>
        Left(
          s"Expected field, got $other with attributes: $attributes, target: $target and name: $name"
        )
    }

  //   final case class FieldFunctionCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]
  implicit def FieldFunctionCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.FieldFunctionCase[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("field_function", attributes, name) =>
        Right(ValueCase.FieldFunctionCase[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected field_function, got $other with attributes: $attributes and name: $name"
        )
    }

  //   final case class IfThenElseCase[+VA, +Self](attributes: VA, condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Nothing, VA, Self]
  implicit def IfThenElseCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.IfThenElseCase[VA, Self]] =
    JsonDecoder.tuple5[String, VA, Self, Self, Self].mapOrFail {
      case ("if_then_else", attributes, condition, thenBranch, elseBranch) =>
        Right(ValueCase.IfThenElseCase[VA, Self](attributes, condition, thenBranch, elseBranch))
      case (other, attributes, condition, thenBranch, elseBranch) =>
        Left(
          s"Expected if_then_else, got $other with attributes: $attributes, condition: $condition, thenBranch: $thenBranch and elseBranch: $elseBranch"
        )
    }

  //   final case class LambdaCase[+VA, +Self](attributes: VA, argumentPattern: Pattern[VA], body: Self) extends ValueCase[Nothing, VA, Self]
  implicit def LambdaCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.LambdaCase[VA, Self]] =
    JsonDecoder.tuple4[String, VA, Pattern[VA], Self].mapOrFail {
      case ("lambda", attributes, argumentPattern, body) =>
        Right(ValueCase.LambdaCase[VA, Self](attributes, argumentPattern, body))
      case (other, attributes, argumentPattern, body) =>
        Left(
          s"Expected lambda, got $other with attributes: $attributes, argumentPattern: $argumentPattern and body: $body"
        )
    }

  // final case class LetDefinitionCase[+TA, +VA, +TypeRepr[+_], +Self](
  //     attributes: VA,
  //     valueName: Name,
  //     valueDefinition: Definition.Case[TA, VA, TypeRepr, Self],
  //     inValue: Self
  // ) extends ValueCase[TA, VA, Self]
  implicit def LetDefinitionCaseValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.LetDefinitionCase[TA, VA, Type, Self]] =
    JsonDecoder.tuple5[String, VA, Name, ValueDefinition.Case[TA, VA, Type, Self], Self].mapOrFail {
      case ("let_definition", attributes, valueName, valueDefinition, inValue) =>
        Right(ValueCase.LetDefinitionCase[TA, VA, Type, Self](attributes, valueName, valueDefinition, inValue))
      case (other, attributes, valueName, valueDefinition, inValue) =>
        Left(
          s"Expected let_definition, got $other with attributes: $attributes, valueName: $valueName, valueDefinition: $valueDefinition and inValue: $inValue"
        )
    }

  // final case class LetRecursionCase[+TA, +VA, +TypeRepr[+_], +Self](
  //     attributes: VA,
  //     valueDefinitions: Map[Name, Definition.Case[TA, VA, TypeRepr, Self]],
  //     inValue: Self
  // ) extends ValueCase[TA, VA, Self]
  implicit def LetRecursionCaseValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.LetRecursionCase[TA, VA, Type, Self]] =
    JsonDecoder.tuple4[String, VA, List[(Name, ValueDefinition.Case[TA, VA, Type, Self])], Self].mapOrFail {
      case ("let_recursion", attributes, valueDefinitions, inValue) =>
        Right(ValueCase.LetRecursionCase[TA, VA, Type, Self](attributes, valueDefinitions.toMap, inValue))
      case (other, attributes, valueDefinitions, inValue) =>
        Left(
          s"Expected let_recursion, got $other with attributes: $attributes, valueDefinitions: $valueDefinitions and inValue: $inValue"
        )
    }

  //   final case class ListCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]
  implicit def ListCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]: JsonDecoder[ValueCase.ListCase[VA, Self]] =
    JsonDecoder.tuple3[String, VA, Chunk[Self]].mapOrFail {
      case ("list", attributes, elements) =>
        Right(ValueCase.ListCase[VA, Self](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected list, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   final case class LiteralCase[+VA, +A](attributes: VA, literal: Literal[A]) extends ValueCase[Nothing, VA, Nothing]
  implicit def LiteralCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.LiteralCase[VA, Any]] =
    JsonDecoder.tuple3[String, VA, Literal[Any]].mapOrFail {
      case ("literal", attributes, literal) =>
        Right(ValueCase.LiteralCase[VA, Any](attributes, literal))
      case (other, attributes, literal) =>
        Left(
          s"Expected literal, got $other with attributes: $attributes and literal: $literal"
        )
    }

  //   final case class PatternMatchCase[+VA, +Self](attributes: VA, branchOutOn: Self, cases: Chunk[(Pattern[VA], Self)]) extends ValueCase[Nothing, VA, Self]
  implicit def PatternMatchCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.PatternMatchCase[VA, Self]] =
    JsonDecoder.tuple4[String, VA, Self, Chunk[(Pattern[VA], Self)]].mapOrFail {
      case ("pattern_match", attributes, branchOutOn, cases) =>
        Right(ValueCase.PatternMatchCase[VA, Self](attributes, branchOutOn, cases))
      case (other, attributes, branchOutOn, cases) =>
        Left(
          s"Expected pattern_match, got $other with attributes: $attributes, branchOutOn: $branchOutOn and cases: $cases"
        )
    }

  // final case class RecordCase[+VA, +Self](attributes: VA, fields: Chunk[(Name, Self)])
  implicit def RecordCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.RecordCase[VA, Self]] =
    JsonDecoder.tuple3[String, VA, Chunk[(Name, Self)]].mapOrFail {
      case ("record", attributes, fields) =>
        Right(ValueCase.RecordCase[VA, Self](attributes, fields))
      case (other, attributes, fields) =>
        Left(
          s"Expected record, got $other with attributes: $attributes and fields: $fields"
        )
    }

  //   final case class ReferenceCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]
  implicit def ReferenceCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.ReferenceCase[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("reference", attributes, name) =>
        Right(ValueCase.ReferenceCase[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected reference, got $other with attributes: $attributes and name: $name"
        )
    }

  //   final case class TupleCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]
  implicit def TupleCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.TupleCase[VA, Self]] =
    JsonDecoder.tuple3[String, VA, Chunk[Self]].mapOrFail {
      case ("Tuple", attributes, elements) =>
        Right(ValueCase.TupleCase[VA, Self](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   final case class UpdateRecordCase[+VA, +Self](attributes: VA, valueToUpdate: Self, fieldsToUpdate: Chunk[(Name, Self)]) extends ValueCase[Nothing, VA, Self]
  implicit def UpdateRecordCaseValueJsonDecoder[VA: JsonDecoder, Self: JsonDecoder]
      : JsonDecoder[ValueCase.UpdateRecordCase[VA, Self]] =
    JsonDecoder.tuple4[String, VA, Self, Chunk[(Name, Self)]].mapOrFail {
      case ("update_record", attributes, valueToUpdate, fieldsToUpdate) =>
        Right(ValueCase.UpdateRecordCase[VA, Self](attributes, valueToUpdate, fieldsToUpdate))
      case (other, attributes, valueToUpdate, fieldsToUpdate) =>
        Left(
          s"Expected update_record, got $other with attributes: $attributes, valueToUpdate: $valueToUpdate and fieldsToUpdate: $fieldsToUpdate"
        )
    }

  implicit def UnitCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.UnitCase[VA]] =
    JsonDecoder.tuple2[String, VA].mapOrFail {
      case ("unit", attributes) =>
        Right(ValueCase.UnitCase[VA](attributes))
      case (other, attributes) =>
        Left(
          s"Expected unit, got $other with attributes: $attributes"
        )
    }

  //   final case class VariableCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]
  implicit def VariableCaseValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[ValueCase.VariableCase[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("variable", attributes, name) =>
        Right(ValueCase.VariableCase[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected variable, got $other with attributes: $attributes and name: $name"
        )
    }

  @nowarn("msg=Implicit resolves to enclosing method valueDecoder")
  implicit def valueDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value[TA, VA]] =
    ConstructorCaseValueJsonDecoder[VA].map(Value(_)) orElse
      FieldFunctionCaseValueJsonDecoder[VA].map(Value(_)) orElse
      LiteralCaseValueJsonDecoder[VA].map(Value(_)) orElse
      ReferenceCaseValueJsonDecoder[VA].map(Value(_)) orElse
      UnitCaseValueJsonDecoder[VA].map(Value(_)) orElse
      VariableCaseValueJsonDecoder[VA].map(Value(_)) orElse
      ApplyCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      DestructureCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      FieldCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      IfThenElseCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      LambdaCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      LetDefinitionCaseValueJsonDecoder[TA, VA, Value[TA, VA]].map(Value(_)) orElse
      LetRecursionCaseValueJsonDecoder[TA, VA, Value[TA, VA]].map(Value(_)) orElse
      ListCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      PatternMatchCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      RecordCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      TupleCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_)) orElse
      UpdateRecordCaseValueJsonDecoder[VA, Value[TA, VA]].map(Value(_))
}

object MorphirJsonDecodingSupport extends MorphirJsonDecodingSupport

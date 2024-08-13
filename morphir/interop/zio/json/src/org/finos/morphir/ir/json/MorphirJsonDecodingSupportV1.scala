package org.finos.morphir.ir
package json

import zio._
import zio.json._
import zio.json.ast.Json
import org.finos.morphir.naming._
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type.{
  Constructors,
  Definition => TypeDefinition,
  Field,
  Specification => TypeSpecification,
  Type
}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir.Value.{Value, _}
import org.finos.morphir.ir.module.{Definition => ModuleDefinition, Specification => ModuleSpecification}

import scala.annotation.{nowarn, unused}

trait MorphirJsonDecodingSupportV1 {
  implicit val unitDecoder: JsonDecoder[Unit] = JsonDecoder.list[String].mapOrFail {
    case a if a.isEmpty => Right(())
    case a              => Left(s"Expected empty list, got [${a.mkString(", ")}]")
  }
  implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
  implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
  implicit val moduleNameDecoder: JsonDecoder[ModuleName]   = pathDecoder.map(ModuleName(_))
  implicit val packageNameDecoder: JsonDecoder[PackageName] = pathDecoder.map(PackageName(_))
  implicit val qNameDecoder: JsonDecoder[QName]             = JsonDecoder.tuple2[Path, Name].map(QName.fromTuple)
  implicit val fqNameDecoder: JsonDecoder[FQName] = JsonDecoder.tuple3[PackageName, ModuleName, Name].map {
    case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
  }

  implicit val qualifiedModuleNameDecoder: JsonDecoder[QualifiedModuleName] =
    JsonDecoder.tuple2[Path, Path].map { case (packageName, modulePath) =>
      QualifiedModuleName(packageName, modulePath)
    }

  implicit def literalBoolDecoder: JsonDecoder[BoolLiteral] =
    JsonDecoder.tuple2[String, Boolean].mapOrFail {
      case ("bool_literal", value) => Right(BoolLiteral(value))
      case (other, value)          => Left(s"Expected bool_literal, got $other with value $value")
    }

  implicit def literalCharDecoder: JsonDecoder[CharLiteral] =
    JsonDecoder.tuple2[String, Char].mapOrFail {
      case ("char_literal", value) => Right(CharLiteral(value))
      case (other, value)          => Left(s"Expected char_literal, got $other with value $value")
    }

  implicit def literalDecimalDecoder: JsonDecoder[DecimalLiteral] =
    JsonDecoder.tuple2[String, java.math.BigDecimal].mapOrFail {
      case ("decimal_literal", value) => Right(DecimalLiteral(value))
      case (other, value)             => Left(s"Expected decimal_literal, got $other with value $value")
    }

  implicit def literalFloatDecoder: JsonDecoder[FloatLiteral] =
    JsonDecoder.tuple2[String, Double].mapOrFail {
      case ("float_literal", value) => Right(FloatLiteral(value))
      case (other, value)           => Left(s"Expected float_literal, got $other with value $value")
    }

  implicit def literalStringDecoder: JsonDecoder[StringLiteral] =
    JsonDecoder.tuple2[String, String].mapOrFail {
      case ("string_literal", value) => Right(StringLiteral(value))
      case (other, value)            => Left(s"Expected string_literal, got $other with value $value")
    }

  implicit def literalWholeNumberDecoder: JsonDecoder[WholeNumberLiteral] =
    JsonDecoder.tuple2[String, Long].mapOrFail {
      case ("int_literal", value) => Right(WholeNumberLiteral(value))
      case (other, value)         => Left(s"Expected int_literal, got $other with value $value")
    }

  implicit def literalDecoder: JsonDecoder[Literal] =
    zio.json.TagBasedParser[Literal] {
      case "bool_literal"    => literalBoolDecoder.widen[Literal]
      case "char_literal"    => literalCharDecoder.widen[Literal]
      case "decimal_literal" => literalDecimalDecoder.widen[Literal]
      case "float_literal"   => literalFloatDecoder.widen[Literal]
      case "string_literal"  => literalStringDecoder.widen[Literal]
      case "int_literal"     => literalWholeNumberDecoder.widen[Literal]
    }

  implicit def fieldDecoder[A: JsonDecoder]: JsonDecoder[Field[A]] =
    JsonDecoder.tuple2[Name, A].map { case (name, fieldType) => Field(name, fieldType) }

  implicit def documentedDecoder[A: JsonDecoder]: JsonDecoder[Documented[A]] =
    JsonDecoder.tuple2[String, A].map { case (doc, value) => Documented(doc, value) }

  implicit def accessControlledDecoder[A: JsonDecoder]: JsonDecoder[AccessControlled[A]] =
    JsonDecoder.tuple2[String, A].map { case (access, value) =>
      AccessControlled(
        access match {
          case "public"  => AccessControlled.Access.Public
          case "private" => AccessControlled.Access.Private
        },
        value
      )
    }

  implicit def extensibleRecordCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.ExtensibleRecord[A]] =
    JsonDecoder.tuple4[String, A, Name, scala.List[Field[Type[A]]]].mapOrFail {
      case ("extensible_record", attributes, name, fields) =>
        Right(Type.ExtensibleRecord(attributes, name, fields))
      case (other, attributes, name, fields) =>
        Left(s"Expected extensible_record, got $other with attributes: $attributes, name: $name and fields: $fields")
    }

  implicit def functionCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Function[A]] =
    JsonDecoder.tuple4[String, A, Type[A], Type[A]].mapOrFail {
      case ("function", attributes, argumentType, returnType) =>
        Right(Type.Function(attributes, argumentType, returnType))
      case (other, attributes, argumentType, returnType) =>
        Left(
          s"Expected function, got $other with attributes: $attributes, argumentType: $argumentType and returnType: $returnType"
        )
    }

  implicit def recordCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Record[A]] =
    JsonDecoder.tuple3[String, A, scala.List[Field[Type[A]]]].mapOrFail {
      case ("record", attributes, fields) =>
        Right(Type.Record(attributes, fields))
      case (other, attributes, fields) =>
        Left(
          s"Expected record, got $other with attributes: $attributes and fields: $fields"
        )
    }

  implicit def referenceCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Reference[A]] =
    JsonDecoder.tuple4[String, A, FQName, scala.List[Type[A]]].mapOrFail {
      case ("reference", attributes, typeName, typeParams) =>
        Right(Type.Reference(attributes, typeName, typeParams))
      case (other, attributes, typeName, typeParams) =>
        Left(
          s"Expected reference, got $other with attributes: $attributes, typeName: $typeName and typeParams: $typeParams"
        )
    }

  implicit def tupleCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Tuple[A]] =
    JsonDecoder.tuple3[String, A, scala.List[Type[A]]].mapOrFail {
      case ("tuple", attributes, elements) =>
        Right(Type.Tuple(attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  implicit def unitCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Unit[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("unit", attributes) =>
        Right(Type.Unit(attributes))
      case (other, attributes) =>
        Left(
          s"Expected unit, got $other with attributes: $attributes"
        )
    }

  implicit def variableCaseTypeDecoder[A: JsonDecoder]: JsonDecoder[Type.Variable[A]] =
    JsonDecoder.tuple3[String, A, Name].mapOrFail {
      case ("variable", attributes, name) =>
        Right(Type.Variable(attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected variable, got $other with attributes: $attributes and name: $name"
        )
    }

  implicit def typeDecoder[A: JsonDecoder]: JsonDecoder[Type[A]] =
    zio.json.TagBasedParser[Type[A]] {
      case "extensible_record" => extensibleRecordCaseTypeDecoder[A].widen
      case "function"          => functionCaseTypeDecoder[A].widen
      case "record"            => recordCaseTypeDecoder[A].widen
      case "reference"         => referenceCaseTypeDecoder[A].widen
      case "tuple"             => tupleCaseTypeDecoder[A].widen
      case "unit"              => unitCaseTypeDecoder[A].widen
      case "variable"          => variableCaseTypeDecoder[A].widen
    }

  implicit def constructorDecoder[A: JsonDecoder]: JsonDecoder[Constructors[A]] =
    JsonDecoder.list[(Name, Chunk[(Name, Type[A])])].map {
      case Nil          => Constructors(Map.empty)
      case constructors => Constructors(constructors.toMap)
    }

  implicit def typeDefinitionTypeAliasDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition.TypeAlias[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Type[A]].mapOrFail {
      case ("type_alias_definition", typeParams, typeExp) =>
        Right(TypeDefinition.TypeAlias(typeParams, typeExp))
      case (other, typeParams, typeExp) =>
        Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and typeExp: $typeExp")
    }

  implicit def typeDefinitionCustomTypeDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition.CustomType[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], AccessControlled[Constructors[A]]].mapOrFail {
      case ("custom_type_definition", typeParams, ctors) =>
        Right(TypeDefinition.CustomType(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and ctors: $ctors")
    }

  implicit def typeDefinitionDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition[A]] =
    zio.json.TagBasedParser[TypeDefinition[A]] {
      case "custom_type_definition" => typeDefinitionCustomTypeDecoder[A].widen
      case "type_alias_definition"  => typeDefinitionTypeAliasDecoder[A].widen
    }

  implicit def typeSpecificationTypeAliasDecoder[A: JsonDecoder]
      : JsonDecoder[TypeSpecification.TypeAliasSpecification[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Type[A]].mapOrFail {
      case ("type_alias_specification", typeParams, expr) =>
        Right(TypeSpecification.TypeAliasSpecification(typeParams, expr))
      case (other, typeParams, expr) =>
        Left(s"Expected type_alias_specification, got $other with typeParams: $typeParams and expr: $expr")
    }

  implicit def typeSpecificationOpaqueTypeDecoder: JsonDecoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonDecoder.tuple2[String, Chunk[Name]].mapOrFail {
      case ("opaque_type_specification", typeParams) =>
        Right(TypeSpecification.OpaqueTypeSpecification(typeParams))
      case (other, typeParams) =>
        Left(s"Expected opaque_type_specification, got $other with typeParams: $typeParams")
    }

  implicit def typeSpecificationCustomTypeDecoder[A: JsonDecoder]
      : JsonDecoder[TypeSpecification.CustomTypeSpecification[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Constructors[A]].mapOrFail {
      case ("custom_type_specification", typeParams, ctors) =>
        Right(TypeSpecification.CustomTypeSpecification(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected custom_type_specification, got $other with typeParams: $typeParams and ctors: $ctors")
    }

  implicit def typeSpecificationDecoder[A: JsonDecoder]: JsonDecoder[TypeSpecification[A]] =
    zio.json.TagBasedParser[TypeSpecification[A]] {
      case "custom_type_specification" => typeSpecificationCustomTypeDecoder[A].widen
      case "opaque_type_specification" => typeSpecificationOpaqueTypeDecoder.widen
      case "type_alias_specification"  => typeSpecificationTypeAliasDecoder[A].widen
    }

  implicit def valueDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[ValueDefinition[TA, VA]] = {
    lazy val dec: JsonDecoder[ValueDefinition[TA, VA]] = DeriveJsonDecoder.gen
    dec
  }

  // implicit def anyDecoder: JsonDecoder[Any] =
  //   Json.Null.decoder.map(v => ())

  implicit def valueSpecificationDecoder[A: JsonDecoder]: JsonDecoder[ValueSpecification[A]] = {
    final case class Spec(inputs: Chunk[(Name, Type[A])], output: Type[A])
    lazy val dec: JsonDecoder[Spec] = DeriveJsonDecoder.gen
    dec.map(spec => ValueSpecification(spec.inputs, spec.output))
  }

  implicit def patternAsPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.AsPattern[A]] =
    JsonDecoder.tuple4[String, A, Pattern[A], Name].mapOrFail {
      case ("as_pattern", attributes, pattern, name) => Right(Pattern.AsPattern(attributes, pattern, name))
      case (other, attributes, pattern, name) =>
        Left(
          s"Expected as_pattern, got $other with attributes: $attributes, pattern: $pattern and name: $name"
        )
    }

  implicit def patternConstructorPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.ConstructorPattern[A]] =
    JsonDecoder.tuple4[String, A, FQName, Chunk[Pattern[A]]].mapOrFail {
      case ("constructor_pattern", attributes, constructorName, argumentPatterns) =>
        Right(Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns))
      case (other, attributes, constructorName, argumentPatterns) =>
        Left(
          s"Expected constructor_pattern, got $other with attributes: $attributes, constructorName: $constructorName and argumentPatterns: $argumentPatterns"
        )
    }

  implicit def patternEmptyListPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.EmptyListPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("empty_list_pattern", attributes) =>
        Right(Pattern.EmptyListPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected empty_list_pattern, got $other with attributes: $attributes")
    }

  implicit def patternHeadTailPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.HeadTailPattern[A]] =
    JsonDecoder.tuple4[String, A, Pattern[A], Pattern[A]].mapOrFail {
      case ("head_tail_pattern", attributes, headPattern, tailPattern) =>
        Right(Pattern.HeadTailPattern(attributes, headPattern, tailPattern))
      case (other, attributes, headPattern, tailPattern) =>
        Left(
          s"Expected head_tail_pattern, got $other with attributes: $attributes, headPattern: $headPattern and tailPattern: $tailPattern"
        )
    }

  implicit def patternLiteralPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.LiteralPattern[A]] =
    JsonDecoder.tuple3[String, A, Literal].mapOrFail {
      case ("literal_pattern", attributes, literal) =>
        Right(Pattern.LiteralPattern(attributes, literal))
      case (other, attributes, literal) =>
        Left(s"Expected literal_pattern, got $other with attributes: $attributes and literal: $literal")
    }

  implicit def patternTuplePatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.TuplePattern[A]] =
    JsonDecoder.tuple3[String, A, Chunk[Pattern[A]]].mapOrFail {
      case ("tuple_pattern", attributes, elementPatterns) =>
        Right(Pattern.TuplePattern(attributes, elementPatterns))
      case (other, attributes, elementPatterns) =>
        Left(s"Expected tuple_pattern, got $other with attributes: $attributes and elementPatterns: $elementPatterns")
    }

  implicit def patternUnitPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.UnitPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("unit_pattern", attributes) =>
        Right(Pattern.UnitPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected unit_pattern, got $other with attributes: $attributes")
    }

  implicit def patternWildcardPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.WildcardPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("wildcard_pattern", attributes) =>
        Right(Pattern.WildcardPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected wildcard_pattern, got $other with attributes: $attributes")
    }

  implicit def patternDecoder[A: JsonDecoder]: JsonDecoder[Pattern[A]] =
    zio.json.TagBasedParser[Pattern[A]] {
      case "as_pattern"          => patternAsPatternDecoder[A].widen
      case "constructor_pattern" => patternConstructorPatternDecoder[A].widen
      case "empty_list_pattern"  => patternEmptyListPatternDecoder[A].widen
      case "head_tail_pattern"   => patternHeadTailPatternDecoder[A].widen
      case "literal_pattern"     => patternLiteralPatternDecoder[A].widen
      case "tuple_pattern"       => patternTuplePatternDecoder[A].widen
      case "unit_pattern"        => patternUnitPatternDecoder[A].widen
      case "wildcard_pattern"    => patternWildcardPatternDecoder[A].widen
    }

  implicit def moduleSpecificationDecoder[TA](implicit
      @unused decoder: JsonDecoder[TA]
  ): JsonDecoder[ModuleSpecification[TA]] = {
    final case class Spec(
        types: List[(Name, Documented[TypeSpecification[TA]])],
        values: List[(Name, Documented[ValueSpecification[TA]])]
    )
    lazy val dec: JsonDecoder[Spec] = DeriveJsonDecoder.gen
    dec.map(spec => ModuleSpecification(spec.types.toMap, spec.values.toMap))
  }

  implicit def moduleDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[ModuleDefinition[TA, VA]] = {
    final case class Def(
        types: List[(Name, AccessControlled[Documented[TypeDefinition[TA]]])],
        values: List[(Name, AccessControlled[Documented[ValueDefinition[TA, VA]]])]
    )
    lazy val dec1: JsonDecoder[Def] = DeriveJsonDecoder.gen
    dec1.map(d => ModuleDefinition(d.types.toMap, d.values.toMap))
  }

  // final case class Specification[+TA](modules: Map[ModuleName, ModuleSpec[TA]]) {
  implicit def packageModuleSpecificationDecoder[TA: JsonDecoder]: JsonDecoder[PackageSpecification[TA]] = {
    final case class Module(name: ModuleName, spec: ModuleSpecification[TA])
    final case class Spec(modules: List[Module])

    implicit val modDec: JsonDecoder[Module] = DeriveJsonDecoder.gen
    lazy val _                               = modDec // This is to suppress unused local val warning
    lazy val dec: JsonDecoder[Spec]          = DeriveJsonDecoder.gen
    dec.map(s => PackageSpecification(s.modules.map(m => m.name -> m.spec).toMap))
  }

  implicit def packageModuleDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]
      : JsonDecoder[PackageDefinition[TA, VA]] = {
    final case class Module(name: ModuleName, `def`: AccessControlled[ModuleDefinition[TA, VA]])
    final case class Spec(modules: List[Module])

    implicit val modDec: JsonDecoder[Module] = DeriveJsonDecoder.gen
    lazy val _                               = modDec // This is to suppress unused local val warning
    lazy val dec: JsonDecoder[Spec]          = DeriveJsonDecoder.gen
    dec.map(d => PackageDefinition(d.modules.map(m => m.name -> m.`def`).toMap))
  }

  //   sealed case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]) extends Value[TA, VA]
  implicit def applyValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Apply[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("apply", attributes, function, argument) =>
        Right(Value.Apply[TA, VA](attributes, function, argument))
      case (other, attributes, function, argument) =>
        Left(
          s"Expected apply, got $other with attributes: $attributes, function: $function and argument: $argument"
        )
    }

  //   sealed case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
  implicit def constructorValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Constructor[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("constructor", attributes, name) =>
        Right(Value.Constructor[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected constructor, got $other with attributes: $attributes and name: $name"
        )
    }

  //   sealed case class Destructure[+TA, +VA](attributes: VA, pattern: Pattern[VA], valueToDestruct: Value[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def destructureValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Destructure[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Pattern[VA], Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("destructure", attributes, pattern, valueToDestruct, inValue) =>
        Right(Value.Destructure[TA, VA](attributes, pattern, valueToDestruct, inValue))
      case (other, attributes, pattern, valueToDestruct, inValue) =>
        Left(
          s"Expected destructure, got $other with attributes: $attributes, pattern: $pattern, valueToDestruct: $valueToDestruct and inValue: $inValue"
        )
    }

  //   sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
  implicit def fieldValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Field[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Name].mapOrFail {
      case ("field", attributes, subjectValue, fieldName) =>
        Right(Value.Field[TA, VA](attributes, subjectValue, fieldName))
      case (other, attributes, subjectValue, fieldName) =>
        Left(
          s"Expected field, got $other with attributes: $attributes, subjectValue: $subjectValue and fieldName: $fieldName"
        )
    }

  //   sealed case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def fieldFunctionValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.FieldFunction[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("field_function", attributes, name) =>
        Right(Value.FieldFunction[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected field_function, got $other with attributes: $attributes and name: $name"
        )
    }

  //   sealed case class IfThenElse[+TA, +VA](attributes: VA, condition: Value[TA, VA], thenBranch: Value[TA, VA], elseBranch: Value[TA, VA]) extends Value[TA, VA]
  implicit def ifThenElseValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.IfThenElse[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Value[TA, VA], Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("if_then_else", attributes, condition, thenBranch, elseBranch) =>
        Right(Value.IfThenElse[TA, VA](attributes, condition, thenBranch, elseBranch))
      case (other, attributes, condition, thenBranch, elseBranch) =>
        Left(
          s"Expected if_then_else, got $other with attributes: $attributes, condition: $condition, thenBranch: $thenBranch and elseBranch: $elseBranch"
        )
    }

  //   sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])  extends Value[TA, VA]
  implicit def lambdaValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Lambda[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Pattern[VA], Value[TA, VA]].mapOrFail {
      case ("lambda", attributes, argumentPattern, body) =>
        Right(Value.Lambda[TA, VA](attributes, argumentPattern, body))
      case (other, attributes, argumentPattern, body) =>
        Left(
          s"Expected lambda, got $other with attributes: $attributes, argumentPattern: $argumentPattern and body: $body"
        )
    }

  //   sealed case class LetDefinition[+TA, +VA](attributes: VA, valueName: Name, valueDefinition: Definition[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letDefinitionValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]
      : JsonDecoder[Value.LetDefinition[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Name, Definition[TA, VA], Value[TA, VA]].mapOrFail {
      case ("let_definition", attributes, valueName, valueDefinition, inValue) =>
        Right(Value.LetDefinition[TA, VA](attributes, valueName, valueDefinition, inValue))
      case (other, attributes, valueName, valueDefinition, inValue) =>
        Left(
          s"Expected let_definition, got $other with attributes: $attributes, valueName: $valueName, valueDefinition: $valueDefinition and inValue: $inValue"
        )
    }

  //   sealed case class LetRecursion[+TA, +VA](attributes: VA, valueDefinitions: Map[Name, Definition[TA, VA]], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letRecursionValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.LetRecursion[TA, VA]] =
    JsonDecoder.tuple4[String, VA, List[(Name, ValueDefinition[TA, VA])], Value[TA, VA]].mapOrFail {
      case ("let_recursion", attributes, valueDefinitions, inValue) =>
        Right(Value.LetRecursion[TA, VA](attributes, valueDefinitions.toMap, inValue))
      case (other, attributes, valueDefinitions, inValue) =>
        Left(
          s"Expected let_recursion, got $other with attributes: $attributes, valueDefinitions: $valueDefinitions and inValue: $inValue"
        )
    }

  //    sealed case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def listValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.List[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[Value[TA, VA]]].mapOrFail {
      case ("list", attributes, elements) =>
        Right(Value.List[TA, VA](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected list, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   sealed case class Literal[+VA](attributes: VA, literal: Lit) extends Value[Nothing, VA]
  implicit def literalValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Literal[VA]] =
    JsonDecoder.tuple3[String, VA, Literal].mapOrFail {
      case ("literal", attributes, literal) =>
        Right(Value.Literal[VA](attributes, literal))
      case (other, attributes, literal) =>
        Left(
          s"Expected literal, got $other with attributes: $attributes and literal: $literal"
        )
    }

  // sealed case class PatternMatch[+TA, +VA](attributes: VA, branchOutOn: Value[TA, VA], cases: Chunk[(Pattern[VA], Value[TA, VA])]) extends Value[TA, VA]
  implicit def patternMatchValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.PatternMatch[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Chunk[(Pattern[VA], Value[TA, VA])]].mapOrFail {
      case ("pattern_match", attributes, branchOutOn, cases) =>
        Right(Value.PatternMatch[TA, VA](attributes, branchOutOn, cases))
      case (other, attributes, branchOutOn, cases) =>
        Left(
          s"Expected pattern_match, got $other with attributes: $attributes, branchOutOn: $branchOutOn and cases: $cases"
        )
    }

  //   sealed case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  implicit def recordValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Record[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[(Name, Value[TA, VA])]].mapOrFail {
      case ("record", attributes, fields) =>
        Right(Value.Record[TA, VA](attributes, fields))
      case (other, attributes, fields) =>
        Left(
          s"Expected record, got $other with attributes: $attributes and fields: $fields"
        )
    }

  //   sealed case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
  implicit def referenceValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Reference[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("reference", attributes, fullyQualifiedName) =>
        Right(Value.Reference[VA](attributes, fullyQualifiedName))
      case (other, attributes, fullyQualifiedName) =>
        Left(
          s"Expected reference, got $other with attributes: $attributes and fullyQualifiedName: $fullyQualifiedName"
        )
    }

  //   sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def tupleValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Tuple[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[Value[TA, VA]]].mapOrFail {
      case ("tuple", attributes, elements) =>
        Right(Value.Tuple[TA, VA](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   sealed case class UpdateRecord[+TA, +VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]]) extends Value[TA, VA]
  implicit def updateRecordValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.UpdateRecord[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], List[(Name, Value[TA, VA])]].mapOrFail {
      case ("update_record", attributes, valueToUpdate, fieldsToUpdate) =>
        Right(Value.UpdateRecord[TA, VA](attributes, valueToUpdate, fieldsToUpdate.toMap))
      case (other, attributes, valueToUpdate, fieldsToUpdate) =>
        Left(
          s"Expected update_record, got $other with attributes: $attributes, valueToUpdate: $valueToUpdate and fieldsToUpdate: $fieldsToUpdate"
        )
    }

  implicit def unitValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Unit[VA]] =
    JsonDecoder.tuple2[String, VA].mapOrFail {
      case ("unit", attributes) =>
        Right(Value.Unit[VA](attributes))
      case (other, attributes) =>
        Left(
          s"Expected unit, got $other with attributes: $attributes"
        )
    }

  //   sealed case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def variableValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Variable[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("variable", attributes, name) =>
        Right(Value.Variable[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected variable, got $other with attributes: $attributes and name: $name"
        )
    }

  implicit def valueDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value[TA, VA]] =
    zio.json.TagBasedParser[Value[TA, VA]] {
      case "constructor"    => constructorValueJsonDecoder[VA].widen
      case "field_function" => fieldFunctionValueJsonDecoder[VA].widen
      case "literal"        => literalValueJsonDecoder[VA].widen
      case "reference"      => referenceValueJsonDecoder[VA].widen
      case "unit"           => unitValueJsonDecoder[VA].widen
      case "variable"       => variableValueJsonDecoder[VA].widen
      case "apply"          => applyValueJsonDecoder[TA, VA].widen
      case "destructure"    => destructureValueJsonDecoder[TA, VA].widen
      case "field"          => fieldValueJsonDecoder[TA, VA].widen
      case "if_then_else"   => ifThenElseValueJsonDecoder[TA, VA].widen
      case "lambda"         => lambdaValueJsonDecoder[TA, VA].widen
      case "let_definition" => letDefinitionValueJsonDecoder[TA, VA].widen
      case "let_recursion"  => letRecursionValueJsonDecoder[TA, VA].widen
      case "list"           => listValueJsonDecoder[TA, VA].widen
      case "pattern_match"  => patternMatchValueJsonDecoder[TA, VA].widen
      case "record"         => recordValueJsonDecoder[TA, VA].widen
      case "tuple"          => tupleValueJsonDecoder[TA, VA].widen
      case "update_record"  => updateRecordValueJsonDecoder[TA, VA].widen
    }

  implicit def distributionLibraryJsonDecoder: JsonDecoder[Library] =
    JsonDecoder
      .tuple4[String, PackageName, List[(PackageName, UPackageSpecification)], PackageDefinition.Typed]
      .mapOrFail {
        case ("library", packageName, dependencies, packageDef) =>
          Right(Library(packageName, dependencies.toMap, packageDef))
        case (other, packageName, dependencies, packageDef) =>
          Left(
            s"Expected library, got $other with packageName: $packageName, dependencies: $dependencies and packageDef: $packageDef"
          )
      }

  implicit def libDecoder: JsonDecoder[Lib] = {
    case class LibTemp(dependencies: List[(PackageName, UPackageSpecification)], packageDef: PackageDefinition.Typed)
    lazy val dec: JsonDecoder[LibTemp] = DeriveJsonDecoder.gen
    dec.map(l => Lib(l.dependencies.map(d => d._1 -> d._2).toMap, l.packageDef))
  }

  implicit def distributionBundleJsonDecoder: JsonDecoder[Bundle] =
    JsonDecoder
      .tuple2[String, List[(PackageName, Lib)]]
      .mapOrFail {
        case ("bundle", libraries) =>
          Right(Bundle(libraries.toMap))
        case (other, libraries) =>
          Left(
            s"Expected bundle, got $other with libraries: $libraries"
          )
      }

  implicit def distributionDecoder: JsonDecoder[Distribution] =
    zio.json.TagBasedParser[Distribution] {
      case "library" => distributionLibraryJsonDecoder.widen[Distribution]
      case "bundle"  => distributionBundleJsonDecoder.widen[Distribution]
    }

  implicit val morphirIRVersionDecoder: JsonDecoder[MorphirIRVersion] = JsonDecoder.int.map {
    case 1 => MorphirIRVersion.V1_0
    case 2 => MorphirIRVersion.V2_0
    case 3 => MorphirIRVersion.V3_0
  }

  implicit def morphirIRFileDecoder: JsonDecoder[MorphirIRFile] = {
    final case class VersionedDistribution(formatVersion: MorphirIRVersion, distribution: Distribution)
    lazy val dec: JsonDecoder[VersionedDistribution] = DeriveJsonDecoder.gen
    dec.map(file => MorphirIRFile(file.formatVersion, file.distribution))
  }
}

object MorphirJsonDecodingSupportV1 extends MorphirJsonDecodingSupportV1

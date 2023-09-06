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
import org.finos.morphir.ir.Type.{Constructors, Type, Definition as TypeDefinition, Specification as TypeSpecification}
import org.finos.morphir.ir.Value.{Definition as ValueDefinition, Specification as ValueSpecification}
import org.finos.morphir.ir.Value.{Value, *}
import org.finos.morphir.ir.module.{Definition as ModuleDefinition, Specification as ModuleSpecification}
import zio.json.JsonDecoder.{JsonError, UnsafeJson}
import zio.json.internal.RetractReader

import scala.annotation.{nowarn, unused}

trait MorphirJsonDecodingSupport {
  implicit val unitDecoder: JsonDecoder[Unit]               = Json.decoder.map(_ => ())
  implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
  implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
  implicit val modulePathDecoder: JsonDecoder[ModuleName]   = pathDecoder.map(ModuleName(_))
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
      case ("BoolLiteral", value) => Right(BoolLiteral(value))
      case (other, value)         => Left(s"Expected BoolLiteral, got $other with value $value")
    }

  implicit def literalCharDecoder: JsonDecoder[CharLiteral] =
    JsonDecoder.tuple2[String, Char].mapOrFail {
      case ("CharLiteral", value) => Right(CharLiteral(value))
      case (other, value)         => Left(s"Expected CharLiteral, got $other with value $value")
    }

  implicit def literalDecimalDecoder: JsonDecoder[DecimalLiteral] =
    JsonDecoder.tuple2[String, java.math.BigDecimal].mapOrFail {
      case ("DecimalLiteral", value) => Right(DecimalLiteral(value))
      case (other, value)            => Left(s"Expected DecimalLiteral, got $other with value $value")
    }

  implicit def literalFloatDecoder: JsonDecoder[FloatLiteral] =
    JsonDecoder.tuple2[String, Double].mapOrFail {
      case ("FloatLiteral", value) => Right(FloatLiteral(value))
      case (other, value)          => Left(s"Expected FloatLiteral, got $other with value $value")
    }

  implicit def literalStringDecoder: JsonDecoder[StringLiteral] =
    JsonDecoder.tuple2[String, String].mapOrFail {
      case ("StringLiteral", value) => Right(StringLiteral(value))
      case (other, value)           => Left(s"Expected StringLiteral, got $other with value $value")
    }

  implicit def literalWholeNumberDecoder: JsonDecoder[WholeNumberLiteral] =
    JsonDecoder.tuple2[String, Long].mapOrFail {
      case ("WholeNumberLiteral", value) => Right(WholeNumberLiteral(value))
      case (other, value)                => Left(s"Expected WholeNumberLiteral, got $other with value $value")
    }

  implicit def literalDecoder: JsonDecoder[Literal] =
    literalBoolDecoder.widen[Literal] orElse
      literalCharDecoder.widen[Literal] orElse
      literalDecimalDecoder.widen[Literal] orElse
      literalFloatDecoder.widen[Literal] orElse
      literalStringDecoder.widen[Literal] orElse
      literalWholeNumberDecoder.widen[Literal]

  implicit def fieldDecoder[A: JsonDecoder]: JsonDecoder[Field[A]] = {
    final case class FieldLike(name: Name, tpe: A)
    lazy val dec: JsonDecoder[FieldLike] = DeriveJsonDecoder.gen
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
        Left(s"Expected TypeAliasDefinition, got $other with typeParams: $typeParams and typeExp: $typeExp")
    }

  implicit def typeDefinitionCustomTypeDecoder[A: JsonDecoder]: JsonDecoder[TypeDefinition.CustomType[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], AccessControlled[Constructors[A]]].mapOrFail {
      case ("CustomTypeDefinition", typeParams, ctors) =>
        Right(TypeDefinition.CustomType(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected CustomTypeDefinition, got $other with typeParams: $typeParams and ctors: $ctors")
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
        Left(s"Expected TypeAliasSpecification, got $other with typeParams: $typeParams and expr: $expr")
    }

  implicit def typeSpecificationOpaqueTypeDecoder: JsonDecoder[TypeSpecification.OpaqueTypeSpecification] =
    JsonDecoder.tuple2[String, Chunk[Name]].mapOrFail {
      case ("OpaqueTypeSpecification", typeParams) =>
        Right(TypeSpecification.OpaqueTypeSpecification(typeParams))
      case (other, typeParams) =>
        Left(s"Expected OpaqueTypeSpecification, got $other with typeParams: $typeParams")
    }

  implicit def typeSpecificationCustomTypeDecoder[A: JsonDecoder]
      : JsonDecoder[TypeSpecification.CustomTypeSpecification[A]] =
    JsonDecoder.tuple3[String, Chunk[Name], Constructors[A]].mapOrFail {
      case ("CustomTypeSpecification", typeParams, ctors) =>
        Right(TypeSpecification.CustomTypeSpecification(typeParams, ctors))
      case (other, typeParams, ctors) =>
        Left(s"Expected CustomTypeSpecification, got $other with typeParams: $typeParams and ctors: $ctors")
    }

  implicit def typeSpecificationDecoder[A: JsonDecoder]: JsonDecoder[TypeSpecification[A]] =
    typeSpecificationTypeAliasDecoder[A].widen[TypeSpecification[A]] orElse
      typeSpecificationCustomTypeDecoder[A].widen[TypeSpecification[A]] orElse
      typeSpecificationOpaqueTypeDecoder.widen[TypeSpecification[A]]

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
      case ("AsPattern", attributes, pattern, name) => Right(Pattern.AsPattern(attributes, pattern, name))
      case (other, attributes, pattern, name) =>
        Left(
          s"Expected AsPattern, got $other with attributes: $attributes, pattern: $pattern and name: $name"
        )
    }

  implicit def patternConstructorPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.ConstructorPattern[A]] =
    JsonDecoder.tuple4[String, A, FQName, Chunk[Pattern[A]]].mapOrFail {
      case ("ConstructorPattern", attributes, constructorName, argumentPatterns) =>
        Right(Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns))
      case (other, attributes, constructorName, argumentPatterns) =>
        Left(
          s"Expected ConstructorPattern, got $other with attributes: $attributes, constructorName: $constructorName and argumentPatterns: $argumentPatterns"
        )
    }

  implicit def patternEmptyListPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.EmptyListPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("EmptyListPattern", attributes) =>
        Right(Pattern.EmptyListPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected EmptyListPattern, got $other with attributes: $attributes")
    }

  implicit def patternHeadTailPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.HeadTailPattern[A]] =
    JsonDecoder.tuple4[String, A, Pattern[A], Pattern[A]].mapOrFail {
      case ("HeadTailPattern", attributes, headPattern, tailPattern) =>
        Right(Pattern.HeadTailPattern(attributes, headPattern, tailPattern))
      case (other, attributes, headPattern, tailPattern) =>
        Left(
          s"Expected HeadTailPattern, got $other with attributes: $attributes, headPattern: $headPattern and tailPattern: $tailPattern"
        )
    }

  implicit def patternLiteralPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.LiteralPattern[A]] =
    JsonDecoder.tuple3[String, A, Literal].mapOrFail {
      case ("LiteralPattern", attributes, literal) =>
        Right(Pattern.LiteralPattern(attributes, literal))
      case (other, attributes, literal) =>
        Left(s"Expected LiteralPattern, got $other with attributes: $attributes and literal: $literal")
    }

  implicit def patternTuplePatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.TuplePattern[A]] =
    JsonDecoder.tuple3[String, A, Chunk[Pattern[A]]].mapOrFail {
      case ("TuplePattern", attributes, elementPatterns) =>
        Right(Pattern.TuplePattern(attributes, elementPatterns))
      case (other, attributes, elementPatterns) =>
        Left(s"Expected TuplePattern, got $other with attributes: $attributes and elementPatterns: $elementPatterns")
    }

  implicit def patternUnitPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.UnitPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("UnitPattern", attributes) =>
        Right(Pattern.UnitPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected UnitPattern, got $other with attributes: $attributes")
    }

  implicit def patternWildcardPatternDecoder[A: JsonDecoder]: JsonDecoder[Pattern.WildcardPattern[A]] =
    JsonDecoder.tuple2[String, A].mapOrFail {
      case ("WildcardPattern", attributes) =>
        Right(Pattern.WildcardPattern[A](attributes))
      case (other, attributes) =>
        Left(s"Expected WildcardPattern, got $other with attributes: $attributes")
    }

  implicit def patternDecoder[A: JsonDecoder]: JsonDecoder[Pattern[A]] =
    patternEmptyListPatternDecoder[A].widen[Pattern[A]] orElse
      patternWildcardPatternDecoder[A].widen[Pattern[A]] orElse
      patternUnitPatternDecoder[A].widen[Pattern[A]] orElse
      patternLiteralPatternDecoder[A].widen[Pattern[A]] orElse
      patternTuplePatternDecoder[A].widen[Pattern[A]] orElse
      patternHeadTailPatternDecoder[A].widen[Pattern[A]] orElse
      patternConstructorPatternDecoder[A].widen[Pattern[A]] orElse
      patternAsPatternDecoder[A].widen[Pattern[A]]

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
    final case class Spec(modules: List[(ModuleName, ModuleSpecification[TA])])
    lazy val dec: JsonDecoder[Spec] = DeriveJsonDecoder.gen
    dec.map(s => PackageSpecification(s.modules.map(m => m._1 -> m._2).toMap))
  }

  implicit def packageModuleDefinitionDecoder[TA: JsonDecoder, VA: JsonDecoder]
      : JsonDecoder[PackageDefinition[TA, VA]] = {
    final case class Spec(modules: List[(ModuleName, AccessControlled[ModuleDefinition[TA, VA]])])
    lazy val dec: JsonDecoder[Spec] = DeriveJsonDecoder.gen
    dec.map(d => PackageDefinition(d.modules.map(m => m._1 -> m._2).toMap))
  }

  //   sealed case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]) extends Value[TA, VA]
  implicit def applyValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Apply[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("Apply", attributes, function, argument) =>
        Right(Value.Apply[TA, VA](attributes, function, argument))
      case (other, attributes, function, argument) =>
        Left(
          s"Expected Apply, got $other with attributes: $attributes, function: $function and argument: $argument"
        )
    }

  //   sealed case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
  implicit def constructorValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Constructor[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("Constructor", attributes, name) =>
        Right(Value.Constructor[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected Constructor, got $other with attributes: $attributes and name: $name"
        )
    }

  //   sealed case class Destructure[+TA, +VA](attributes: VA, pattern: Pattern[VA], valueToDestruct: Value[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def destructureValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Destructure[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Pattern[VA], Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("Destructure", attributes, pattern, valueToDestruct, inValue) =>
        Right(Value.Destructure[TA, VA](attributes, pattern, valueToDestruct, inValue))
      case (other, attributes, pattern, valueToDestruct, inValue) =>
        Left(
          s"Expected Destructure, got $other with attributes: $attributes, pattern: $pattern, valueToDestruct: $valueToDestruct and inValue: $inValue"
        )
    }

  //   sealed case class Field[+TA, +VA](attributes: VA, subjectValue: Value[TA, VA], fieldName: Name) extends Value[TA, VA]
  implicit def fieldValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Field[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Name].mapOrFail {
      case ("Field", attributes, subjectValue, fieldName) =>
        Right(Value.Field[TA, VA](attributes, subjectValue, fieldName))
      case (other, attributes, subjectValue, fieldName) =>
        Left(
          s"Expected Field, got $other with attributes: $attributes, subjectValue: $subjectValue and fieldName: $fieldName"
        )
    }

  //   sealed case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def fieldFunctionValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.FieldFunction[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("FieldFunction", attributes, name) =>
        Right(Value.FieldFunction[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected FieldFunction, got $other with attributes: $attributes and name: $name"
        )
    }

  //   sealed case class IfThenElse[+TA, +VA](attributes: VA, condition: Value[TA, VA], thenBranch: Value[TA, VA], elseBranch: Value[TA, VA]) extends Value[TA, VA]
  implicit def ifThenElseValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.IfThenElse[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Value[TA, VA], Value[TA, VA], Value[TA, VA]].mapOrFail {
      case ("IfThenElse", attributes, condition, thenBranch, elseBranch) =>
        Right(Value.IfThenElse[TA, VA](attributes, condition, thenBranch, elseBranch))
      case (other, attributes, condition, thenBranch, elseBranch) =>
        Left(
          s"Expected IfThenElse, got $other with attributes: $attributes, condition: $condition, thenBranch: $thenBranch and elseBranch: $elseBranch"
        )
    }

  //   sealed case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])  extends Value[TA, VA]
  implicit def lambdaValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Lambda[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Pattern[VA], Value[TA, VA]].mapOrFail {
      case ("Lambda", attributes, argumentPattern, body) =>
        Right(Value.Lambda[TA, VA](attributes, argumentPattern, body))
      case (other, attributes, argumentPattern, body) =>
        Left(
          s"Expected Lambda, got $other with attributes: $attributes, argumentPattern: $argumentPattern and body: $body"
        )
    }

  //   sealed case class LetDefinition[+TA, +VA](attributes: VA, valueName: Name, valueDefinition: Definition[TA, VA], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letDefinitionValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]
      : JsonDecoder[Value.LetDefinition[TA, VA]] =
    JsonDecoder.tuple5[String, VA, Name, Definition[TA, VA], Value[TA, VA]].mapOrFail {
      case ("LetDefinition", attributes, valueName, valueDefinition, inValue) =>
        Right(Value.LetDefinition[TA, VA](attributes, valueName, valueDefinition, inValue))
      case (other, attributes, valueName, valueDefinition, inValue) =>
        Left(
          s"Expected LetDefinition, got $other with attributes: $attributes, valueName: $valueName, valueDefinition: $valueDefinition and inValue: $inValue"
        )
    }

  //   sealed case class LetRecursion[+TA, +VA](attributes: VA, valueDefinitions: Map[Name, Definition[TA, VA]], inValue: Value[TA, VA]) extends Value[TA, VA]
  implicit def letRecursionValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.LetRecursion[TA, VA]] =
    JsonDecoder.tuple4[String, VA, List[(Name, ValueDefinition[TA, VA])], Value[TA, VA]].mapOrFail {
      case ("LetRecursion", attributes, valueDefinitions, inValue) =>
        Right(Value.LetRecursion[TA, VA](attributes, valueDefinitions.toMap, inValue))
      case (other, attributes, valueDefinitions, inValue) =>
        Left(
          s"Expected LetRecursion, got $other with attributes: $attributes, valueDefinitions: $valueDefinitions and inValue: $inValue"
        )
    }

  //    sealed case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def listValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.List[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[Value[TA, VA]]].mapOrFail {
      case ("List", attributes, elements) =>
        Right(Value.List[TA, VA](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected List, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   sealed case class Literal[+VA](attributes: VA, literal: Lit) extends Value[Nothing, VA]
  implicit def literalValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Literal[VA]] =
    JsonDecoder.tuple3[String, VA, Literal].mapOrFail {
      case ("Literal", attributes, literal) =>
        Right(Value.Literal[VA](attributes, literal))
      case (other, attributes, literal) =>
        Left(
          s"Expected Literal, got $other with attributes: $attributes and literal: $literal"
        )
    }

  // sealed case class PatternMatch[+TA, +VA](attributes: VA, branchOutOn: Value[TA, VA], cases: Chunk[(Pattern[VA], Value[TA, VA])]) extends Value[TA, VA]
  implicit def patternMatchValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.PatternMatch[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], Chunk[(Pattern[VA], Value[TA, VA])]].mapOrFail {
      case ("PatternMatch", attributes, branchOutOn, cases) =>
        Right(Value.PatternMatch[TA, VA](attributes, branchOutOn, cases))
      case (other, attributes, branchOutOn, cases) =>
        Left(
          s"Expected PatternMatch, got $other with attributes: $attributes, branchOutOn: $branchOutOn and cases: $cases"
        )
    }

  //   sealed case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]
  implicit def recordValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Record[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[(Name, Value[TA, VA])]].mapOrFail {
      case ("Record", attributes, fields) =>
        Right(Value.Record[TA, VA](attributes, fields))
      case (other, attributes, fields) =>
        Left(
          s"Expected Record, got $other with attributes: $attributes and fields: $fields"
        )
    }

  //   sealed case class Reference[+VA](attributes: VA, fullyQualifiedName: FQName) extends Value[Nothing, VA]
  implicit def referenceValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Reference[VA]] =
    JsonDecoder.tuple3[String, VA, FQName].mapOrFail {
      case ("Reference", attributes, fullyQualifiedName) =>
        Right(Value.Reference[VA](attributes, fullyQualifiedName))
      case (other, attributes, fullyQualifiedName) =>
        Left(
          s"Expected Reference, got $other with attributes: $attributes and fullyQualifiedName: $fullyQualifiedName"
        )
    }

  //   sealed case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]
  implicit def tupleValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.Tuple[TA, VA]] =
    JsonDecoder.tuple3[String, VA, Chunk[Value[TA, VA]]].mapOrFail {
      case ("Tuple", attributes, elements) =>
        Right(Value.Tuple[TA, VA](attributes, elements))
      case (other, attributes, elements) =>
        Left(
          s"Expected Tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  //   sealed case class UpdateRecord[+TA, +VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]]) extends Value[TA, VA]
  implicit def updateRecordValueJsonDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value.UpdateRecord[TA, VA]] =
    JsonDecoder.tuple4[String, VA, Value[TA, VA], List[(Name, Value[TA, VA])]].mapOrFail {
      case ("UpdateRecord", attributes, valueToUpdate, fieldsToUpdate) =>
        Right(Value.UpdateRecord[TA, VA](attributes, valueToUpdate, fieldsToUpdate.toMap))
      case (other, attributes, valueToUpdate, fieldsToUpdate) =>
        Left(
          s"Expected UpdateRecord, got $other with attributes: $attributes, valueToUpdate: $valueToUpdate and fieldsToUpdate: $fieldsToUpdate"
        )
    }

  implicit def unitValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Unit[VA]] =
    JsonDecoder.tuple2[String, VA].mapOrFail {
      case ("Unit", attributes) =>
        Right(Value.Unit[VA](attributes))
      case (other, attributes) =>
        Left(
          s"Expected Unit, got $other with attributes: $attributes"
        )
    }

  //   sealed case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  implicit def variableValueJsonDecoder[VA: JsonDecoder]: JsonDecoder[Value.Variable[VA]] =
    JsonDecoder.tuple3[String, VA, Name].mapOrFail {
      case ("Variable", attributes, name) =>
        Right(Value.Variable[VA](attributes, name))
      case (other, attributes, name) =>
        Left(
          s"Expected Variable, got $other with attributes: $attributes and name: $name"
        )
    }

  implicit def valueDecoder[TA: JsonDecoder, VA: JsonDecoder]: JsonDecoder[Value[TA, VA]] =
    zio.json.TagBasedParser[Value[TA, VA]] {
      case "Constructor"   => constructorValueJsonDecoder[VA].widen
      case "FieldFunction" => fieldFunctionValueJsonDecoder[VA].widen
      case "Literal"       => literalValueJsonDecoder[VA].widen
      case "Reference"     => referenceValueJsonDecoder[VA].widen
      case "Unit"          => unitValueJsonDecoder[VA].widen
      case "Variable"      => variableValueJsonDecoder[VA].widen
      case "Apply"         => applyValueJsonDecoder[TA, VA].widen
      case "Destructure"   => destructureValueJsonDecoder[TA, VA].widen
      case "Field"         => fieldValueJsonDecoder[TA, VA].widen
      case "IfThenElse"    => ifThenElseValueJsonDecoder[TA, VA].widen
      case "Lambda"        => lambdaValueJsonDecoder[TA, VA].widen
      case "LetDefinition" => letDefinitionValueJsonDecoder[TA, VA].widen
      case "LetRecursion"  => letRecursionValueJsonDecoder[TA, VA].widen
      case "List"          => listValueJsonDecoder[TA, VA].widen
      case "PatternMatch"  => patternMatchValueJsonDecoder[TA, VA].widen
      case "Record"        => recordValueJsonDecoder[TA, VA].widen
      case "Tuple"         => tupleValueJsonDecoder[TA, VA].widen
      case "UpdateRecord"  => updateRecordValueJsonDecoder[TA, VA].widen
    }

  implicit def distributionLibraryJsonDecoder: JsonDecoder[Library] =
    JsonDecoder
      .tuple4[String, PackageName, List[(PackageName, UPackageSpecification)], PackageDefinition.Typed]
      .mapOrFail {
        case ("Library", packageName, dependencies, packageDef) =>
          Right(Library(packageName, dependencies.toMap, packageDef))
        case (other, packageName, dependencies, packageDef) =>
          Left(
            s"Expected Library, got $other with packageName: $packageName, dependencies: $dependencies and packageDef: $packageDef"
          )
      }

  implicit def distributionDecoder: JsonDecoder[Distribution] =
    distributionLibraryJsonDecoder.widen[Distribution]

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

object MorphirJsonDecodingSupport extends MorphirJsonDecodingSupport

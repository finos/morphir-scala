package org.finos.morphir.ir.json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write

import org.finos.morphir.naming._
import org.finos.morphir.ir.v4._
import org.finos.morphir.ir.v4.Type
import org.finos.morphir.ir.v4.Value
import org.finos.morphir.ir.v4.Pattern

trait MorphirJsonEncodingSupportV4 extends JsonEncodingHelpers {

  // --- Supporting Encoders ---
  implicit val nameEncoder: JsonEncoder[Name]               = JsonEncoder.list[String].contramap(_.toList)
  implicit val pathEncoder: JsonEncoder[Path]               = JsonEncoder.list[Name].contramap(_.toList)
  implicit val packageNameEncoder: JsonEncoder[PackageName] = pathEncoder.contramap(_.toPath)
  implicit val moduleNameEncoder: JsonEncoder[ModuleName]   = pathEncoder.contramap(_.toPath)
  implicit val fqNameEncoder: JsonEncoder[FQName]           =
    JsonEncoder.tuple3[PackageName, ModuleName, Name].contramap(fq => (fq.packagePath, fq.modulePath, fq.localName))

  implicit val sourceLocationEncoder: JsonEncoder[SourceLocation] = DeriveJsonEncoder.gen[SourceLocation]

  // FQName as a key in Map needs a FieldEncoder. Using string representation.
  implicit val fqNameFieldEncoder: JsonFieldEncoder[FQName] = JsonFieldEncoder[String].contramap(_.toString)

  // Helper to encode JSON AST safely
  private def encodeObj(obj: Json.Obj, indent: Option[Int], out: Write): Unit =
    Json.encoder.unsafeEncode(obj, indent, out)

  // Custom encoder for TypeAttributes to omit empty fields
  implicit val typeAttributesEncoder: JsonEncoder[TypeAttributes] = new JsonEncoder[TypeAttributes] {
    def unsafeEncode(a: TypeAttributes, indent: Option[Int], out: Write): Unit = {
      var fields = List.empty[(String, Json)]
      a.source.foreach(s => fields = ("source" -> s.toJsonAST.getOrElse(Json.Null)) :: fields)
      a.constraints.foreach(c => fields = ("constraints" -> c.toJsonAST.getOrElse(Json.Null)) :: fields)
      if (a.extensions.nonEmpty) fields = ("extensions" -> a.extensions.toJsonAST.getOrElse(Json.Null)) :: fields

      if (fields.isEmpty) out.write("{}")
      else encodeObj(Json.Obj(fields.reverse: _*), indent, out)
    }
  }

  // Custom encoder for ValueAttributes
  implicit val valueAttributesEncoder: JsonEncoder[ValueAttributes] = new JsonEncoder[ValueAttributes] {
    def unsafeEncode(a: ValueAttributes, indent: Option[Int], out: Write): Unit = {
      var fields = List.empty[(String, Json)]
      a.source.foreach(s => fields = ("source" -> s.toJsonAST.getOrElse(Json.Null)) :: fields)
      a.inferredType.foreach(t => fields = ("inferredType" -> t.toJsonAST.getOrElse(Json.Null)) :: fields)
      a.properties.foreach(p => fields = ("properties" -> p.toJsonAST.getOrElse(Json.Null)) :: fields)
      if (a.extensions.nonEmpty) fields = ("extensions" -> a.extensions.toJsonAST.getOrElse(Json.Null)) :: fields

      if (fields.isEmpty) out.write("{}")
      else encodeObj(Json.Obj(fields.reverse: _*), indent, out)
    }
  }

  // --- Constraint & Property Encoders ---
  implicit val intWidthEncoder: JsonEncoder[IntWidth]                         = DeriveJsonEncoder.gen
  implicit val floatWidthEncoder: JsonEncoder[FloatWidth]                     = DeriveJsonEncoder.gen
  implicit val numericConstraintEncoder: JsonEncoder[NumericConstraint]       = DeriveJsonEncoder.gen
  implicit val stringEncodingEncoder: JsonEncoder[StringEncoding]             = DeriveJsonEncoder.gen
  implicit val stringConstraintEncoder: JsonEncoder[StringConstraint]         = DeriveJsonEncoder.gen
  implicit val collectionConstraintEncoder: JsonEncoder[CollectionConstraint] = DeriveJsonEncoder.gen
  implicit val customConstraintEncoder: JsonEncoder[CustomConstraint]         = DeriveJsonEncoder.gen
  implicit val typeConstraintsEncoder: JsonEncoder[TypeConstraints]           = DeriveJsonEncoder.gen

  implicit val purityEncoder: JsonEncoder[Purity]                   = DeriveJsonEncoder.gen
  implicit val valuePropertiesEncoder: JsonEncoder[ValueProperties] = DeriveJsonEncoder.gen

  // --- Type Encoders ---
  // Using explicit encoders to match V4 spec object format: { "CaseName": { ...fields... } }

  implicit val fieldEncoder: JsonEncoder[Field] = DeriveJsonEncoder.gen

  implicit lazy val typeEncoder: JsonEncoder[Type] = new JsonEncoder[Type] {
    def unsafeEncode(t: Type, indent: Option[Int], out: Write): Unit = t match {
      case Type.Variable(attr, name) =>
        encodeObj(
          Json.Obj("Variable" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "name"       -> name.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.Reference(attr, fqName, args) =>
        encodeObj(
          Json.Obj("Reference" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fqName"     -> fqName.toJsonAST.getOrElse(Json.Null),
            "args"       -> args.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.Tuple(attr, elements) =>
        encodeObj(
          Json.Obj("Tuple" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "elements"   -> elements.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.Record(attr, fields) =>
        encodeObj(
          Json.Obj("Record" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fields"     -> fields.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.ExtensibleRecord(attr, variable, fields) =>
        encodeObj(
          Json.Obj("ExtensibleRecord" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "variable"   -> variable.toJsonAST.getOrElse(Json.Null),
            "fields"     -> fields.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.Function(attr, arg, ret) =>
        encodeObj(
          Json.Obj("Function" -> Json.Obj(
            "attributes"   -> attr.toJsonAST.getOrElse(Json.Null),
            "argumentType" -> arg.toJsonAST.getOrElse(Json.Null),
            "returnType"   -> ret.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Type.Unit(attr) =>
        encodeObj(
          Json.Obj("Unit" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
    }
  }

  // --- Pattern Encoder ---
  implicit lazy val patternEncoder: JsonEncoder[Pattern] = new JsonEncoder[Pattern] {
    def unsafeEncode(p: Pattern, indent: Option[Int], out: Write): Unit = p match {
      case Pattern.WildcardPattern(attr) =>
        encodeObj(
          Json.Obj("WildcardPattern" -> Json.Obj("attributes" -> attr.toJsonAST.getOrElse(Json.Null))),
          indent,
          out
        )
      case Pattern.AsPattern(attr, pattern, name) =>
        encodeObj(
          Json.Obj("AsPattern" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "pattern"    -> pattern.toJsonAST.getOrElse(Json.Null),
            "name"       -> name.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Pattern.TuplePattern(attr, elements) =>
        encodeObj(
          Json.Obj("TuplePattern" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "elements"   -> elements.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Pattern.ConstructorPattern(attr, constructor, args) =>
        encodeObj(
          Json.Obj("ConstructorPattern" -> Json.Obj(
            "attributes"  -> attr.toJsonAST.getOrElse(Json.Null),
            "constructor" -> constructor.toJsonAST.getOrElse(Json.Null),
            "args"        -> args.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Pattern.EmptyListPattern(attr) =>
        encodeObj(
          Json.Obj("EmptyListPattern" -> Json.Obj("attributes" -> attr.toJsonAST.getOrElse(Json.Null))),
          indent,
          out
        )
      case Pattern.HeadTailPattern(attr, head, tail) =>
        encodeObj(
          Json.Obj("HeadTailPattern" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "head"       -> head.toJsonAST.getOrElse(Json.Null),
            "tail"       -> tail.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Pattern.LiteralPattern(attr, lit) =>
        encodeObj(
          Json.Obj("LiteralPattern" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "literal"    -> lit.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Pattern.UnitPattern(attr) =>
        encodeObj(Json.Obj("UnitPattern" -> Json.Obj("attributes" -> attr.toJsonAST.getOrElse(Json.Null))), indent, out)
    }
  }

  // --- Value Encoder ---
  implicit lazy val valueEncoder: JsonEncoder[Value] = new JsonEncoder[Value] {
    def unsafeEncode(v: Value, indent: Option[Int], out: Write): Unit = v match {
      case Value.Literal(attr, lit) =>
        encodeObj(
          Json.Obj("Literal" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "literal"    -> lit.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Constructor(attr, fqName) =>
        encodeObj(
          Json.Obj("Constructor" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fqName"     -> fqName.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Tuple(attr, elements) =>
        encodeObj(
          Json.Obj("Tuple" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "elements"   -> elements.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.List(attr, items) =>
        encodeObj(
          Json.Obj("List" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "items"      -> items.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Record(attr, fields) =>
        encodeObj(
          Json.Obj("Record" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fields"     -> fields.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Unit(attr) =>
        encodeObj(Json.Obj("Unit" -> Json.Obj("attributes" -> attr.toJsonAST.getOrElse(Json.Null))), indent, out)
      case Value.Variable(attr, name) =>
        encodeObj(
          Json.Obj("Variable" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "name"       -> name.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Reference(attr, fqName) =>
        encodeObj(
          Json.Obj("Reference" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fqName"     -> fqName.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Field(attr, record, fieldName) =>
        encodeObj(
          Json.Obj("Field" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "record"     -> record.toJsonAST.getOrElse(Json.Null),
            "fieldName"  -> fieldName.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.FieldFunction(attr, fieldName) =>
        encodeObj(
          Json.Obj("FieldFunction" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fieldName"  -> fieldName.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Apply(attr, function, argument) =>
        encodeObj(
          Json.Obj("Apply" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "function"   -> function.toJsonAST.getOrElse(Json.Null),
            "argument"   -> argument.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Lambda(attr, argumentPattern, body) =>
        encodeObj(
          Json.Obj("Lambda" -> Json.Obj(
            "attributes"      -> attr.toJsonAST.getOrElse(Json.Null),
            "argumentPattern" -> argumentPattern.toJsonAST.getOrElse(Json.Null),
            "body"            -> body.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.LetDefinition(attr, name, definition, inValue) =>
        encodeObj(
          Json.Obj("LetDefinition" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "name"       -> name.toJsonAST.getOrElse(Json.Null),
            "definition" -> definition.toJsonAST.getOrElse(Json.Null),
            "inValue"    -> inValue.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.LetRecursion(attr, bindings, inValue) =>
        encodeObj(
          Json.Obj("LetRecursion" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "bindings"   -> bindings.toJsonAST.getOrElse(Json.Null),
            "inValue"    -> inValue.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Destructure(attr, pattern, valueToDestructure, inValue) =>
        encodeObj(
          Json.Obj("Destructure" -> Json.Obj(
            "attributes"         -> attr.toJsonAST.getOrElse(Json.Null),
            "pattern"            -> pattern.toJsonAST.getOrElse(Json.Null),
            "valueToDestructure" -> valueToDestructure.toJsonAST.getOrElse(Json.Null),
            "inValue"            -> inValue.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.IfThenElse(attr, condition, thenBranch, elseBranch) =>
        encodeObj(
          Json.Obj("IfThenElse" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "condition"  -> condition.toJsonAST.getOrElse(Json.Null),
            "thenBranch" -> thenBranch.toJsonAST.getOrElse(Json.Null),
            "elseBranch" -> elseBranch.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.PatternMatch(attr, subject, cases) =>
        encodeObj(
          Json.Obj("PatternMatch" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "subject"    -> subject.toJsonAST.getOrElse(Json.Null),
            "cases"      -> cases.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.UpdateRecord(attr, record, updates) =>
        encodeObj(
          Json.Obj("UpdateRecord" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "record"     -> record.toJsonAST.getOrElse(Json.Null),
            "updates"    -> updates.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Hole(attr, reason, expectedType) =>
        encodeObj(
          Json.Obj("Hole" -> Json.Obj(
            "attributes"   -> attr.toJsonAST.getOrElse(Json.Null),
            "reason"       -> reason.toJsonAST.getOrElse(Json.Null),
            "expectedType" -> expectedType.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.Native(attr, fqName, nativeInfo) =>
        encodeObj(
          Json.Obj("Native" -> Json.Obj(
            "attributes" -> attr.toJsonAST.getOrElse(Json.Null),
            "fqName"     -> fqName.toJsonAST.getOrElse(Json.Null),
            "nativeInfo" -> nativeInfo.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
      case Value.External(attr, externalName, targetPlatform) =>
        encodeObj(
          Json.Obj("External" -> Json.Obj(
            "attributes"     -> attr.toJsonAST.getOrElse(Json.Null),
            "externalName"   -> externalName.toJsonAST.getOrElse(Json.Null),
            "targetPlatform" -> targetPlatform.toJsonAST.getOrElse(Json.Null)
          )),
          indent,
          out
        )
    }
  }

  // --- Incompleteness & HoleReason Encoders ---
  implicit val holeReasonEncoder: JsonEncoder[HoleReason]                   = DeriveJsonEncoder.gen
  implicit val incompletenessEncoder: JsonEncoder[Incompleteness]           = DeriveJsonEncoder.gen
  implicit val valueDefinitionBodyEncoder: JsonEncoder[ValueDefinitionBody] = DeriveJsonEncoder.gen
  implicit val nativeInfoEncoder: JsonEncoder[NativeInfo]                   = DeriveJsonEncoder.gen
  implicit val nativeHintEncoder: JsonEncoder[NativeHint]                   = DeriveJsonEncoder.gen
  implicit val literalEncoder: JsonEncoder[Literal]                         = new JsonEncoder[Literal] {
    def unsafeEncode(l: Literal, indent: Option[Int], out: Write): Unit = l match {
      case Literal.BoolLiteral(value) =>
        encodeObj(Json.Obj("BoolLiteral" -> Json.Obj("value" -> Json.Bool(value))), indent, out)
      case Literal.CharLiteral(value) =>
        encodeObj(Json.Obj("CharLiteral" -> Json.Obj("value" -> Json.Str(value))), indent, out)
      case Literal.StringLiteral(value) =>
        encodeObj(Json.Obj("StringLiteral" -> Json.Obj("value" -> Json.Str(value))), indent, out)
      case Literal.IntegerLiteral(value) =>
        encodeObj(Json.Obj("IntegerLiteral" -> Json.Obj("value" -> Json.Str(value.toString))), indent, out)
      case Literal.FloatLiteral(value) =>
        encodeObj(Json.Obj("FloatLiteral" -> Json.Obj("value" -> Json.Num(value))), indent, out)
      case Literal.DecimalLiteral(value) =>
        encodeObj(Json.Obj("DecimalLiteral" -> Json.Obj("value" -> Json.Num(value))), indent, out)
    }
  }
}

object MorphirJsonEncodingSupportV4 extends MorphirJsonEncodingSupportV4

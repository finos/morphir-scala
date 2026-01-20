package org.finos.morphir.ir.json

import zio._
import zio.json._
import zio.json.ast.Json
import org.finos.morphir.naming._
import org.finos.morphir.ir.v4._
import org.finos.morphir.ir.v4.Type
import org.finos.morphir.ir.v4.Value
import org.finos.morphir.ir.v4.Pattern

trait MorphirJsonDecodingSupportV4 {

  // --- Supporting Decoders ---
  implicit val sourceLocationDecoder: JsonDecoder[SourceLocation]             = DeriveJsonDecoder.gen
  implicit val intWidthDecoder: JsonDecoder[IntWidth]                         = DeriveJsonDecoder.gen
  implicit val floatWidthDecoder: JsonDecoder[FloatWidth]                     = DeriveJsonDecoder.gen
  implicit val numericConstraintDecoder: JsonDecoder[NumericConstraint]       = DeriveJsonDecoder.gen
  implicit val stringEncodingDecoder: JsonDecoder[StringEncoding]             = DeriveJsonDecoder.gen
  implicit val stringConstraintDecoder: JsonDecoder[StringConstraint]         = DeriveJsonDecoder.gen
  implicit val collectionConstraintDecoder: JsonDecoder[CollectionConstraint] = DeriveJsonDecoder.gen
  implicit val customConstraintDecoder: JsonDecoder[CustomConstraint]         = DeriveJsonDecoder.gen
  implicit val typeConstraintsDecoder: JsonDecoder[TypeConstraints]           = DeriveJsonDecoder.gen

  implicit val purityDecoder: JsonDecoder[Purity]                   = DeriveJsonDecoder.gen
  implicit val valuePropertiesDecoder: JsonDecoder[ValueProperties] = DeriveJsonDecoder.gen

  implicit val nativeHintDecoder: JsonDecoder[NativeHint] = DeriveJsonDecoder.gen
  implicit val nativeInfoDecoder: JsonDecoder[NativeInfo] = DeriveJsonDecoder.gen

  implicit val fieldDecoder: JsonDecoder[Field] = DeriveJsonDecoder.gen

  // Basic types decoders
  implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
  implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
  implicit val packageNameDecoder: JsonDecoder[PackageName] = pathDecoder.map(PackageName(_))
  implicit val moduleNameDecoder: JsonDecoder[ModuleName]   = pathDecoder.map(ModuleName(_))
  implicit val fqNameDecoder: JsonDecoder[FQName]           = JsonDecoder.tuple3[PackageName, ModuleName, Name].map {
    case (p, m, n) => FQName(p, m, n)
  }

  // Helper to decode from Json AST Object
  private def decodeObj[A](f: Json.Obj => Either[String, A]): JsonDecoder[A] =
    JsonDecoder[Json].mapOrFail {
      case obj: Json.Obj => f(obj)
      case other         => Left(s"Expected JSON Object, got $other")
    }

  private def getField[A](obj: Json.Obj, name: String)(implicit decoder: JsonDecoder[A]): Either[String, A] =
    obj.get(name).toRight(s"Missing field '$name'").flatMap(_.as[A])

  private def getOptionalField[A](obj: Json.Obj, name: String)(implicit
      decoder: JsonDecoder[A]
  ): Either[String, Option[A]] =
    obj.get(name) match {
      case Some(json) => json.as[A].map(Some(_))
      case None       => Right(None)
    }

  // --- Attributes Decoders ---
  implicit val fqNameFieldDecoder: JsonFieldDecoder[FQName]         = JsonFieldDecoder[String].map(FQName.fromString(_))
  implicit val nameFieldDecoder: JsonFieldDecoder[Name]             = JsonFieldDecoder[String].map(Name.fromString(_))
  implicit val moduleNameFieldDecoder: JsonFieldDecoder[ModuleName] =
    JsonFieldDecoder[String].map(s => ModuleName(Path.fromString(s)))
  implicit val packageNameFieldDecoder: JsonFieldDecoder[PackageName] =
    JsonFieldDecoder[String].map(s => PackageName(Path.fromString(s)))

  implicit val typeAttributesDecoder: JsonDecoder[TypeAttributes] = decodeObj { obj =>
    for {
      source      <- getOptionalField[SourceLocation](obj, "source")
      constraints <- getOptionalField[TypeConstraints](obj, "constraints")
      extensions  <- getOptionalField[Map[FQName, Value]](obj, "extensions").map(_.getOrElse(Map.empty))
    } yield TypeAttributes(source, constraints, extensions)
  }

  implicit val valueAttributesDecoder: JsonDecoder[ValueAttributes] = decodeObj { obj =>
    for {
      source       <- getOptionalField[SourceLocation](obj, "source")
      inferredType <- getOptionalField[Type](obj, "inferredType")
      properties   <- getOptionalField[ValueProperties](obj, "properties")
      extensions   <- getOptionalField[Map[FQName, Value]](obj, "extensions").map(_.getOrElse(Map.empty))
    } yield ValueAttributes(source, inferredType, properties, extensions)
  }

  // --- Literal Decoder ---
  implicit val literalDecoder: JsonDecoder[Literal] = decodeObj { obj =>
    if (obj.fields.isEmpty) Left("Empty object for Literal")
    else {
      val (kind, data) = obj.fields.head
      data.as[Json.Obj].flatMap { innerObj =>
        kind match {
          case "BoolLiteral" =>
            innerObj.get("value").toRight("Missing value").flatMap(_.as[Boolean].map(Literal.BoolLiteral(_)))
          case "CharLiteral" =>
            innerObj.get("value").toRight("Missing value").flatMap(_.as[String].map(Literal.CharLiteral(_)))
          case "StringLiteral" =>
            innerObj.get("value").toRight("Missing value").flatMap(_.as[String].map(Literal.StringLiteral(_)))
          case "IntegerLiteral" => innerObj.get("value").toRight("Missing value").flatMap(_.as[String].map(s =>
              Literal.IntegerLiteral(BigInt(s))
            ))
          case "FloatLiteral" =>
            innerObj.get("value").toRight("Missing value").flatMap(_.as[Double].map(Literal.FloatLiteral(_)))
          case "DecimalLiteral" =>
            innerObj.get("value").toRight("Missing value").flatMap(_.as[BigDecimal].map(Literal.DecimalLiteral(_)))
          case _ => Left(s"Unknown Literal kind: $kind")
        }
      }
    }
  }

  // --- Type Decoder ---
  implicit lazy val typeDecoder: JsonDecoder[Type] = decodeObj { obj =>
    if (obj.fields.isEmpty) Left("Empty object for Type")
    else {
      val (kind, data) = obj.fields.head
      data.as[Json.Obj].flatMap { inner =>
        // Decode attributes safely
        val attrRes = inner.get("attributes") match {
          case Some(json) => json.as[TypeAttributes]
          case None       => Right(TypeAttributes.empty)
        }

        attrRes.flatMap { attr =>
          kind match {
            case "Variable" =>
              inner.get("name").toRight("element 'name' missing").flatMap(_.as[Name]).map(Type.Variable(attr, _))
            case "Reference" =>
              for {
                fqName <- inner.get("fqName").toRight("element 'fqName' missing").flatMap(_.as[FQName])
                args   <- inner.get("args").toRight("element 'args' missing").flatMap(_.as[Chunk[Type]])
              } yield Type.Reference(attr, fqName, args)
            case "Tuple" =>
              inner.get("elements").toRight("element 'elements' missing").flatMap(_.as[Chunk[Type]]).map(Type.Tuple(
                attr,
                _
              ))
            case "Record" =>
              inner.get("fields").toRight("element 'fields' missing").flatMap(_.as[Chunk[Field]]).map(Type.Record(
                attr,
                _
              ))
            case "ExtensibleRecord" =>
              for {
                variable <- inner.get("variable").toRight("element 'variable' missing").flatMap(_.as[Name])
                fields   <- inner.get("fields").toRight("element 'fields' missing").flatMap(_.as[Chunk[Field]])
              } yield Type.ExtensibleRecord(attr, variable, fields)
            case "Function" =>
              for {
                arg <- inner.get("argumentType").toRight("element 'argumentType' missing").flatMap(_.as[Type])
                ret <- inner.get("returnType").toRight("element 'returnType' missing").flatMap(_.as[Type])
              } yield Type.Function(attr, arg, ret)
            case "Unit" => Right(Type.Unit(attr))
            case _      => Left(s"Unknown Type kind: $kind")
          }
        }
      }
    }
  }

  // --- Pattern Decoder ---
  implicit lazy val patternDecoder: JsonDecoder[Pattern] = decodeObj { obj =>
    if (obj.fields.isEmpty) Left("Empty object for Pattern")
    else {
      val (kind, data) = obj.fields.head
      data.as[Json.Obj].flatMap { inner =>
        val attrRes = inner.get("attributes") match {
          case Some(json) => json.as[ValueAttributes]
          case None       => Right(ValueAttributes.empty)
        }

        attrRes.flatMap { attr =>
          kind match {
            case "WildcardPattern" => Right(Pattern.WildcardPattern(attr))
            case "AsPattern"       =>
              for {
                p <- inner.get("pattern").toRight("missing 'pattern'").flatMap(_.as[Pattern])
                n <- inner.get("name").toRight("missing 'name'").flatMap(_.as[Name])
              } yield Pattern.AsPattern(attr, p, n)
            case "TuplePattern" =>
              inner.get(
                "elements"
              ).toRight("missing 'elements'").flatMap(_.as[Chunk[Pattern]]).map(Pattern.TuplePattern(attr, _))
            case "ConstructorPattern" =>
              for {
                ctor <- inner.get("constructor").toRight("missing 'constructor'").flatMap(_.as[FQName])
                args <- inner.get("args").toRight("missing 'args'").flatMap(_.as[Chunk[Pattern]])
              } yield Pattern.ConstructorPattern(attr, ctor, args)
            case "EmptyListPattern" => Right(Pattern.EmptyListPattern(attr))
            case "HeadTailPattern"  =>
              for {
                h <- inner.get("head").toRight("missing 'head'").flatMap(_.as[Pattern])
                t <- inner.get("tail").toRight("missing 'tail'").flatMap(_.as[Pattern])
              } yield Pattern.HeadTailPattern(attr, h, t)
            case "LiteralPattern" =>
              inner.get("literal").toRight("missing 'literal'").flatMap(_.as[Literal]).map(Pattern.LiteralPattern(
                attr,
                _
              ))
            case "UnitPattern" => Right(Pattern.UnitPattern(attr))
            case _             => Left(s"Unknown Pattern kind: $kind")
          }
        }
      }
    }
  }

  // --- Incompleteness & HoleReason Decoders ---
  implicit val holeReasonDecoder: JsonDecoder[HoleReason]                   = DeriveJsonDecoder.gen
  implicit val incompletenessDecoder: JsonDecoder[Incompleteness]           = DeriveJsonDecoder.gen
  implicit val valueDefinitionBodyDecoder: JsonDecoder[ValueDefinitionBody] = DeriveJsonDecoder.gen

  // --- Value Decoder ---
  implicit lazy val valueDecoder: JsonDecoder[Value] = decodeObj { obj =>
    if (obj.fields.isEmpty) Left("Empty object for Value")
    else {
      val (kind, data) = obj.fields.head
      data.as[Json.Obj].flatMap { inner =>
        val attrRes = inner.get("attributes") match {
          case Some(json) => json.as[ValueAttributes]
          case None       => Right(ValueAttributes.empty)
        }

        attrRes.flatMap { attr =>
          kind match {
            case "Literal" =>
              inner.get("literal").toRight("missing 'literal'").flatMap(_.as[Literal]).map(Value.Literal(attr, _))
            case "Variable" =>
              inner.get("name").toRight("missing 'name'").flatMap(_.as[Name]).map(Value.Variable(attr, _))
            case "Reference" =>
              inner.get("fqName").toRight("missing 'fqName'").flatMap(_.as[FQName]).map(Value.Reference(attr, _))
            case "Tuple" =>
              inner.get("elements").toRight("missing 'elements'").flatMap(_.as[Chunk[Value]]).map(Value.Tuple(attr, _))
            case "List" =>
              inner.get("items").toRight("missing 'items'").flatMap(_.as[Chunk[Value]]).map(Value.List(attr, _))
            case "Record" =>
              inner.get("fields").toRight("missing 'fields'").flatMap(_.as[Chunk[(Name, Value)]]).map(Value.Record(
                attr,
                _
              ))
            case "Constructor" =>
              inner.get("fqName").toRight("missing 'fqName'").flatMap(_.as[FQName]).map(Value.Constructor(attr, _))
            case "Unit"  => Right(Value.Unit(attr))
            case "Field" =>
              for {
                rec   <- inner.get("record").toRight("missing 'record'").flatMap(_.as[Value])
                fname <- inner.get("fieldName").toRight("missing 'fieldName'").flatMap(_.as[Name])
              } yield Value.Field(attr, rec, fname)
            case "FieldFunction" =>
              inner.get("fieldName").toRight("missing 'fieldName'").flatMap(_.as[Name]).map(Value.FieldFunction(
                attr,
                _
              ))
            case "Apply" =>
              for {
                fn  <- inner.get("function").toRight("missing 'function'").flatMap(_.as[Value])
                arg <- inner.get("argument").toRight("missing 'argument'").flatMap(_.as[Value])
              } yield Value.Apply(attr, fn, arg)
            case "Lambda" =>
              for {
                arg  <- inner.get("argumentPattern").toRight("missing 'argumentPattern'").flatMap(_.as[Pattern])
                body <- inner.get("body").toRight("missing 'body'").flatMap(_.as[Value])
              } yield Value.Lambda(attr, arg, body)
            case "LetDefinition" =>
              for {
                name <- inner.get("name").toRight("missing 'name'").flatMap(_.as[Name])
                defn <- inner.get("definition").toRight("missing 'definition'").flatMap(_.as[ValueDefinitionBody])
                inv  <- inner.get("inValue").toRight("missing 'inValue'").flatMap(_.as[Value])
              } yield Value.LetDefinition(attr, name, defn, inv)
            case "LetRecursion" =>
              for {
                binds <-
                  inner.get("bindings").toRight("missing 'bindings'").flatMap(_.as[Chunk[(Name, ValueDefinitionBody)]])
                inv <- inner.get("inValue").toRight("missing 'inValue'").flatMap(_.as[Value])
              } yield Value.LetRecursion(attr, binds, inv)
            case "Destructure" =>
              for {
                pat       <- inner.get("pattern").toRight("missing 'pattern'").flatMap(_.as[Pattern])
                valToDest <-
                  inner.get("valueToDestructure").toRight("missing 'valueToDestructure'").flatMap(_.as[Value])
                inv <- inner.get("inValue").toRight("missing 'inValue'").flatMap(_.as[Value])
              } yield Value.Destructure(attr, pat, valToDest, inv)
            case "IfThenElse" =>
              for {
                cond <- inner.get("condition").toRight("missing 'condition'").flatMap(_.as[Value])
                thn  <- inner.get("thenBranch").toRight("missing 'thenBranch'").flatMap(_.as[Value])
                els  <- inner.get("elseBranch").toRight("missing 'elseBranch'").flatMap(_.as[Value])
              } yield Value.IfThenElse(attr, cond, thn, els)
            case "PatternMatch" =>
              for {
                subj  <- inner.get("subject").toRight("missing 'subject'").flatMap(_.as[Value])
                cases <- inner.get("cases").toRight("missing 'cases'").flatMap(_.as[Chunk[(Pattern, Value)]])
              } yield Value.PatternMatch(attr, subj, cases)
            case "UpdateRecord" =>
              for {
                rec <- inner.get("record").toRight("missing 'record'").flatMap(_.as[Value])
                upd <- inner.get("updates").toRight("missing 'updates'").flatMap(_.as[Chunk[(Name, Value)]])
              } yield Value.UpdateRecord(attr, rec, upd)
            case "Hole" =>
              for {
                reason   <- inner.get("reason").toRight("missing 'reason'").flatMap(_.as[HoleReason])
                expected <- inner.get("expectedType").toRight("missing 'expectedType'").flatMap(_.as[Option[Type]])
              } yield Value.Hole(attr, reason, expected)
            case "Native" =>
              for {
                fqn  <- inner.get("fqName").toRight("missing 'fqName'").flatMap(_.as[FQName])
                info <- inner.get("nativeInfo").toRight("missing 'nativeInfo'").flatMap(_.as[NativeInfo])
              } yield Value.Native(attr, fqn, info)
            case "External" =>
              for {
                name <- inner.get("externalName").toRight("missing 'externalName'").flatMap(_.as[String])
                plat <- inner.get("targetPlatform").toRight("missing 'targetPlatform'").flatMap(_.as[String])
              } yield Value.External(attr, name, plat)
            case _ => Left(s"Unknown Value kind: $kind")
          }
        }
      }
    }
  }

  // --- Access & Documented Decoders ---
  implicit val accessDecoder: JsonDecoder[Access]                                        = DeriveJsonDecoder.gen
  implicit def accessControlledDecoder[A: JsonDecoder]: JsonDecoder[AccessControlled[A]] = DeriveJsonDecoder.gen
  implicit val documentationDecoder: JsonDecoder[Documentation]                          = DeriveJsonDecoder.gen
  implicit def documentedDecoder[A: JsonDecoder]: JsonDecoder[Documented[A]]             = DeriveJsonDecoder.gen

  // --- TypeDefinition Decoders ---
  implicit val constructorDecoder: JsonDecoder[Constructor] = DeriveJsonDecoder.gen
  implicit val derivedTypeSpecificationDetailsDecoder: JsonDecoder[DerivedTypeSpecificationDetails] =
    DeriveJsonDecoder.gen
  implicit val typeDefinitionDecoder: JsonDecoder[TypeDefinition] = DeriveJsonDecoder.gen

  // --- TypeSpecification Decoders ---
  implicit val typeSpecificationDecoder: JsonDecoder[TypeSpecification] = DeriveJsonDecoder.gen

  // --- ValueDefinition/Specification Decoders ---
  implicit val valueDefinitionDecoder: JsonDecoder[ValueDefinition]       = DeriveJsonDecoder.gen
  implicit val valueSpecificationDecoder: JsonDecoder[ValueSpecification] = DeriveJsonDecoder.gen

  // --- Distribution Decoders ---
  implicit val packageInfoDecoder: JsonDecoder[PackageInfo] = DeriveJsonDecoder.gen

  // ModuleDefinition requires explicit decoder as it uses AccessControlled[Documented[...]]
  // which might be complex for auto derivation if not strictly matching case class.
  // But our types are case classes using these generics, so DeriveJsonDecoder might work
  // IF the implicit for AccessControlled[Documented[...]] is picked up.
  // We defined: implicit def accessControlledDecoder[A] and documentedDecoder[A].
  // So it should work.
  implicit val moduleDefinitionDecoder: JsonDecoder[ModuleDefinition]       = DeriveJsonDecoder.gen
  implicit val moduleSpecificationDecoder: JsonDecoder[ModuleSpecification] = DeriveJsonDecoder.gen

  implicit val packageDefinitionDecoder: JsonDecoder[PackageDefinition]       = DeriveJsonDecoder.gen
  implicit val packageSpecificationDecoder: JsonDecoder[PackageSpecification] = DeriveJsonDecoder.gen

  implicit val libraryDistributionDecoder: JsonDecoder[LibraryDistribution] = DeriveJsonDecoder.gen
  implicit val specsDistributionDecoder: JsonDecoder[SpecsDistribution]     = DeriveJsonDecoder.gen
  // EntryPoint types needed for ApplicationDistribution
  implicit val entryPointKindDecoder: JsonDecoder[EntryPointKind]                   = DeriveJsonDecoder.gen
  implicit val entryPointDecoder: JsonDecoder[EntryPoint]                           = DeriveJsonDecoder.gen
  implicit val applicationDistributionDecoder: JsonDecoder[ApplicationDistribution] = DeriveJsonDecoder.gen

  implicit val distributionDecoder: JsonDecoder[Distribution] = DeriveJsonDecoder.gen

}

object MorphirJsonDecodingSupportV4 extends MorphirJsonDecodingSupportV4

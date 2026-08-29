package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.Json.given_Json
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering

enum V3ProjectionError derives CanEqual:
  case UnsupportedDistribution(kind: String)
  case UnsupportedFeature(path: String, feature: String)
  case InvalidModel(path: String, details: String)

object V3WireProjection:
  private type Projection[A] = Result[V3ProjectionError, A]
  private type Value         = Structure.Value

  def project(distribution: cm.Distribution): Projection[Value] =
    distribution match
      case cm.Distribution.Library(library) =>
        mapResult(projectLibrary(library)) { projected =>
          record(
            "formatVersion" -> integer(3),
            "distribution"  -> projected
          )
        }
      case _: cm.Distribution.Specs       => fail(V3ProjectionError.UnsupportedDistribution("Specs"))
      case _: cm.Distribution.Application => fail(V3ProjectionError.UnsupportedDistribution("Application"))

  def encode(distribution: cm.Distribution): Projection[String] =
    mapResult(project(distribution))(Json.encode(_))

  private def projectLibrary(library: cm.LibraryDistribution): Projection[Value] =
    flatMapResult(projectDependencies(library.dependencies, "distribution.dependencies")) { dependencies =>
      mapResult(projectPackageDefinition(library.definition, "distribution.definition")) { definition =>
        sequence(
          str("Library"),
          packageName(library.packageInfo.name),
          dependencies,
          definition
        )
      }
    }

  private def projectDependencies(
      dependencies: Map[PackageName, cm.PackageSpecification],
      path: String
  ): Projection[Value] =
    val entries = dependencies.toList.sortBy(_._1.toPath.segments.toList.map(_.toList))
    mapResult(traverse(entries) { case (dependencyName, specification) =>
      mapResult(projectPackageSpecification(specification, s"$path.${dependencyName.toString}")) { projected =>
        sequence(packageName(dependencyName), projected)
      }
    })(values => sequence(values*))

  private def projectPackageDefinition(definition: cm.PackageDefinition, path: String): Projection[Value] =
    val modules = definition.modules.toList.sortBy(_._1.toPath.segments.toList.map(_.toList))
    mapResult(traverse(modules) { case (module, accessControlled) =>
      mapResult(projectAccessControlled(accessControlled, s"$path.modules.${module.toString}")(
        projectModuleDefinition
      )) { projected =>
        sequence(moduleName(module), projected)
      }
    })(projected => record("modules" -> sequence(projected*)))

  private def projectPackageSpecification(specification: cm.PackageSpecification, path: String): Projection[Value] =
    val modules = specification.modules.toList.sortBy(_._1.toPath.segments.toList.map(_.toList))
    mapResult(traverse(modules) { case (module, definition) =>
      mapResult(projectModuleSpecification(definition, s"$path.modules.${module.toString}")) { projected =>
        sequence(moduleName(module), projected)
      }
    })(projected => record("modules" -> sequence(projected*)))

  private def projectModuleDefinition(definition: cm.ModuleDefinition, path: String): Projection[Value] =
    flatMapResult(projectTypeDefinitions(definition.types, s"$path.types")) { types =>
      mapResult(projectValueDefinitions(definition.values, s"$path.values")) { values =>
        record("types" -> types, "values" -> values)
      }
    }

  private def projectModuleSpecification(specification: cm.ModuleSpecification, path: String): Projection[Value] =
    flatMapResult(projectTypeSpecifications(specification.types, s"$path.types")) { types =>
      mapResult(projectValueSpecifications(specification.values, s"$path.values")) { values =>
        record("types" -> types, "values" -> values)
      }
    }

  private def projectTypeDefinitions(
      definitions: Map[Name, cm.AccessControlled[cm.Documented[cm.TypeDefinition]]],
      path: String
  ): Projection[Value] =
    val entries = definitions.toList.sortBy(_._1.toList)
    mapResult(traverse(entries) { case (definitionName, accessControlled) =>
      mapResult(projectAccessControlled(accessControlled, s"$path.${definitionName.toString}")((documented, itemPath) =>
        projectDocumented(documented, itemPath)(projectTypeDefinition)
      )) { projected =>
        sequence(name(definitionName), projected)
      }
    })(values => sequence(values*))

  private def projectValueDefinitions(
      definitions: Map[Name, cm.AccessControlled[cm.Documented[cm.ValueDefinition]]],
      path: String
  ): Projection[Value] =
    val entries = definitions.toList.sortBy(_._1.toList)
    mapResult(traverse(entries) { case (definitionName, accessControlled) =>
      val itemPath = s"$path.${definitionName.toString}"
      flatMapResult(validateValueAccess(accessControlled, itemPath)) { _ =>
        mapResult(projectAccessControlled(accessControlled, itemPath)((documented, documentedPath) =>
          projectDocumented(documented, documentedPath) { (definition, definitionPath) =>
            projectValueDefinitionBody(definition.body.value, definitionPath)
          }
        )) { projected =>
          sequence(name(definitionName), projected)
        }
      }
    })(values => sequence(values*))

  private def projectTypeSpecifications(
      specifications: Map[Name, cm.Documented[cm.TypeSpecification]],
      path: String
  ): Projection[Value] =
    val entries = specifications.toList.sortBy(_._1.toList)
    mapResult(traverse(entries) { case (specificationName, documented) =>
      mapResult(projectDocumented(documented, s"$path.${specificationName.toString}")(projectTypeSpecification)) {
        projected => sequence(name(specificationName), projected)
      }
    })(values => sequence(values*))

  private def projectValueSpecifications(
      specifications: Map[Name, cm.Documented[cm.ValueSpecification]],
      path: String
  ): Projection[Value] =
    val entries = specifications.toList.sortBy(_._1.toList)
    mapResult(traverse(entries) { case (specificationName, documented) =>
      mapResult(projectDocumented(documented, s"$path.${specificationName.toString}")(projectValueSpecification)) {
        projected => sequence(name(specificationName), projected)
      }
    })(values => sequence(values*))

  private def validateValueAccess(
      value: cm.AccessControlled[cm.Documented[cm.ValueDefinition]],
      path: String
  ): Projection[Unit] =
    if value.access == value.value.value.body.access then succeed(())
    else fail(V3ProjectionError.InvalidModel(path, "value access markers disagree"))

  private def projectAccessControlled[A](
      value: cm.AccessControlled[A],
      path: String
  )(projectValue: (A, String) => Projection[Value]): Projection[Value] =
    mapResult(projectValue(value.value, s"$path.value")) { projected =>
      record(
        "access" -> access(value.access),
        "value"  -> projected
      )
    }

  private def projectDocumented[A](
      documented: cm.Documented[A],
      path: String
  )(projectValue: (A, String) => Projection[Value]): Projection[Value] =
    mapResult(projectValue(documented.value, s"$path.value")) { projected =>
      record(
        "doc"   -> str(documented.doc.fold("")(_.lines.mkString("\n"))),
        "value" -> projected
      )
    }

  private def projectTypeDefinition(definition: cm.TypeDefinition, path: String): Projection[Value] =
    definition match
      case cm.TypeDefinition.TypeAliasDefinition(params, body) =>
        mapResult(projectType(body, s"$path.body")) { projectedBody =>
          sequence(str("TypeAliasDefinition"), sequence(params.map(name)*), projectedBody)
        }
      case cm.TypeDefinition.CustomTypeDefinition(params, constructors) =>
        mapResult(projectAccessControlled(constructors, s"$path.constructors")(projectConstructors)) {
          projectedConstructors =>
            sequence(str("CustomTypeDefinition"), sequence(params.map(name)*), projectedConstructors)
        }
      case _: cm.TypeDefinition.IncompleteTypeDefinition =>
        fail(V3ProjectionError.UnsupportedFeature(path, "IncompleteTypeDefinition"))

  private def projectTypeSpecification(specification: cm.TypeSpecification, path: String): Projection[Value] =
    specification match
      case cm.TypeSpecification.TypeAliasSpecification(params, body) =>
        mapResult(projectType(body, s"$path.body")) { projectedBody =>
          sequence(str("TypeAliasSpecification"), sequence(params.map(name)*), projectedBody)
        }
      case cm.TypeSpecification.OpaqueTypeSpecification(params) =>
        succeed(sequence(str("OpaqueTypeSpecification"), sequence(params.map(name)*)))
      case cm.TypeSpecification.CustomTypeSpecification(params, constructors) =>
        mapResult(projectConstructors(constructors, s"$path.constructors")) { projectedConstructors =>
          sequence(str("CustomTypeSpecification"), sequence(params.map(name)*), projectedConstructors)
        }
      case _: cm.TypeSpecification.DerivedTypeSpecification =>
        fail(V3ProjectionError.UnsupportedFeature(path, "DerivedTypeSpecification"))

  private def projectConstructors(constructors: Chunk[cm.Constructor], path: String): Projection[Value] =
    mapResult(traverse(constructors.toList.zipWithIndex) { case (constructor, index) =>
      mapResult(projectParameters(constructor.args, s"$path[$index].args", typed = false)) { args =>
        sequence(name(constructor.name), args)
      }
    })(values => sequence(values*))

  private def projectValueSpecification(specification: cm.ValueSpecification, path: String): Projection[Value] =
    flatMapResult(projectParameters(specification.inputs, s"$path.inputs", typed = false)) { inputs =>
      mapResult(projectType(specification.output, s"$path.output")) { output =>
        record("inputs" -> inputs, "output" -> output)
      }
    }

  private def projectValueDefinitionBody(body: cm.ValueDefinitionBody, path: String): Projection[Value] =
    body match
      case cm.ValueDefinitionBody.ExpressionBody(inputTypes, outputType, expression) =>
        flatMapResult(projectParameters(inputTypes, s"$path.inputTypes", typed = true)) { inputs =>
          flatMapResult(projectType(outputType, s"$path.outputType")) { output =>
            mapResult(projectExpr(expression, s"$path.body")) { projectedBody =>
              record(
                "inputTypes" -> inputs,
                "outputType" -> output,
                "body"       -> projectedBody
              )
            }
          }
        }
      case _: cm.ValueDefinitionBody.NativeBody =>
        fail(V3ProjectionError.UnsupportedFeature(path, "NativeBody"))
      case _: cm.ValueDefinitionBody.ExternalBody =>
        fail(V3ProjectionError.UnsupportedFeature(path, "ExternalBody"))
      case _: cm.ValueDefinitionBody.IncompleteBody =>
        fail(V3ProjectionError.UnsupportedFeature(path, "IncompleteBody"))

  private def projectParameters(parameters: Chunk[cm.Parameter], path: String, typed: Boolean): Projection[Value] =
    mapResult(traverse(parameters.toList.zipWithIndex) { case (parameter, index) =>
      mapResult(projectType(parameter.tpe, s"$path[$index].type")) { projectedType =>
        if typed then sequence(name(parameter.name), projectedType, projectedType)
        else sequence(name(parameter.name), projectedType)
      }
    })(values => sequence(values*))

  private def projectType(tpe: cm.Type, path: String): Projection[Value] =
    flatMapResult(projectTypeAttributes(tpe, path)) { attributes =>
      tpe match
        case cm.Type.Variable(_, variableName) =>
          succeed(sequence(str("Variable"), attributes, name(variableName)))
        case cm.Type.Reference(_, reference, args) =>
          mapResult(traverse(args.toList.zipWithIndex) { case (argument, index) =>
            projectType(argument, s"$path.args[$index]")
          })(projected => sequence(str("Reference"), attributes, fqName(reference), sequence(projected*)))
        case cm.Type.Tuple(_, elements) =>
          mapResult(traverse(elements.toList.zipWithIndex) { case (element, index) =>
            projectType(element, s"$path.elements[$index]")
          })(projected => sequence(str("Tuple"), attributes, sequence(projected*)))
        case cm.Type.Record(_, fields) =>
          mapResult(projectTypeFields(fields, s"$path.fields"))(projected =>
            sequence(str("Record"), attributes, projected)
          )
        case cm.Type.ExtensibleRecord(_, variable, fields) =>
          mapResult(projectTypeFields(fields, s"$path.fields"))(projected =>
            sequence(str("ExtensibleRecord"), attributes, name(variable), projected)
          )
        case cm.Type.Function(_, argumentType, returnType) =>
          flatMapResult(projectType(argumentType, s"$path.argument")) { argument =>
            mapResult(projectType(returnType, s"$path.return")) { result =>
              sequence(str("Function"), attributes, argument, result)
            }
          }
        case cm.Type.Unit(_) => succeed(sequence(str("Unit"), attributes))
    }

  private def projectTypeAttributes(tpe: cm.Type, path: String): Projection[Value] =
    val attributes = tpe match
      case cm.Type.Variable(value, _)            => value
      case cm.Type.Reference(value, _, _)        => value
      case cm.Type.Tuple(value, _)               => value
      case cm.Type.Record(value, _)              => value
      case cm.Type.ExtensibleRecord(value, _, _) => value
      case cm.Type.Function(value, _, _)         => value
      case cm.Type.Unit(value)                   => value
    if attributes == cm.TypeAttributes.empty then succeed(record())
    else fail(V3ProjectionError.UnsupportedFeature(path, "TypeAttributes"))

  private def projectTypeFields(fields: Chunk[cm.Field], path: String): Projection[Value] =
    mapResult(traverse(fields.toList.zipWithIndex) { case (field, index) =>
      mapResult(projectType(field.fieldType, s"$path[$index].type")) { projectedType =>
        record("name" -> name(field.name), "tpe" -> projectedType)
      }
    })(values => sequence(values*))

  private def projectExpr(expression: cm.Expr, path: String): Projection[Value] =
    flatMapResult(projectValueAttributes(expression, path)) { attributes =>
      expression match
        case cm.Expr.Literal(_, literal) =>
          mapResult(projectLiteral(literal, s"$path.literal"))(value =>
            sequence(str("Literal"), attributes, value)
          )
        case cm.Expr.Constructor(_, reference) =>
          succeed(sequence(str("Constructor"), attributes, fqName(reference)))
        case cm.Expr.Tuple(_, elements) =>
          mapResult(projectExpressions(elements, s"$path.elements"))(values =>
            sequence(str("Tuple"), attributes, values)
          )
        case cm.Expr.List(_, items) =>
          mapResult(projectExpressions(items, s"$path.items"))(values =>
            sequence(str("List"), attributes, values)
          )
        case cm.Expr.Record(_, fields) =>
          mapResult(projectRecordFields(fields, s"$path.fields"))(values =>
            sequence(str("Record"), attributes, values)
          )
        case cm.Expr.Unit(_)                   => succeed(sequence(str("Unit"), attributes))
        case cm.Expr.Variable(_, variableName) =>
          succeed(sequence(str("Variable"), attributes, name(variableName)))
        case cm.Expr.Reference(_, reference) =>
          succeed(sequence(str("Reference"), attributes, fqName(reference)))
        case cm.Expr.Field(_, subject, fieldName) =>
          mapResult(projectExpr(subject, s"$path.record"))(projected =>
            sequence(str("Field"), attributes, projected, name(fieldName))
          )
        case cm.Expr.FieldFunction(_, fieldName) =>
          succeed(sequence(str("FieldFunction"), attributes, name(fieldName)))
        case cm.Expr.Apply(_, function, argument) =>
          flatMapResult(projectExpr(function, s"$path.function")) { projectedFunction =>
            mapResult(projectExpr(argument, s"$path.argument")) { projectedArgument =>
              sequence(str("Apply"), attributes, projectedFunction, projectedArgument)
            }
          }
        case cm.Expr.Lambda(_, argumentPattern, body) =>
          flatMapResult(projectPattern(argumentPattern, s"$path.argumentPattern")) { pattern =>
            mapResult(projectExpr(body, s"$path.body"))(projectedBody =>
              sequence(str("Lambda"), attributes, pattern, projectedBody)
            )
          }
        case cm.Expr.LetDefinition(_, valueName, definition, inValue) =>
          flatMapResult(projectValueDefinitionBody(definition, s"$path.definition")) { projectedDefinition =>
            mapResult(projectExpr(inValue, s"$path.inValue"))(projectedInValue =>
              sequence(str("LetDefinition"), attributes, name(valueName), projectedDefinition, projectedInValue)
            )
          }
        case cm.Expr.LetRecursion(_, bindings, inValue) =>
          flatMapResult(projectBindings(bindings, s"$path.bindings")) { projectedBindings =>
            mapResult(projectExpr(inValue, s"$path.inValue"))(projectedInValue =>
              sequence(str("LetRecursion"), attributes, projectedBindings, projectedInValue)
            )
          }
        case cm.Expr.Destructure(_, pattern, valueToDestructure, inValue) =>
          flatMapResult(projectPattern(pattern, s"$path.pattern")) { projectedPattern =>
            flatMapResult(projectExpr(valueToDestructure, s"$path.value")) { projectedValue =>
              mapResult(projectExpr(inValue, s"$path.inValue"))(projectedInValue =>
                sequence(str("Destructure"), attributes, projectedPattern, projectedValue, projectedInValue)
              )
            }
          }
        case cm.Expr.IfThenElse(_, condition, thenBranch, elseBranch) =>
          flatMapResult(projectExpr(condition, s"$path.condition")) { projectedCondition =>
            flatMapResult(projectExpr(thenBranch, s"$path.then")) { projectedThen =>
              mapResult(projectExpr(elseBranch, s"$path.else"))(projectedElse =>
                sequence(str("IfThenElse"), attributes, projectedCondition, projectedThen, projectedElse)
              )
            }
          }
        case cm.Expr.PatternMatch(_, subject, cases) =>
          flatMapResult(projectExpr(subject, s"$path.subject")) { projectedSubject =>
            mapResult(projectMatchCases(cases, s"$path.cases"))(projectedCases =>
              sequence(str("PatternMatch"), attributes, projectedSubject, projectedCases)
            )
          }
        case cm.Expr.UpdateRecord(_, recordValue, updates) =>
          flatMapResult(projectExpr(recordValue, s"$path.record")) { projectedRecord =>
            mapResult(projectRecordFields(updates, s"$path.updates"))(projectedUpdates =>
              sequence(str("UpdateRecord"), attributes, projectedRecord, projectedUpdates)
            )
          }
        case _: cm.Expr.Hole     => fail(V3ProjectionError.UnsupportedFeature(path, "Hole"))
        case _: cm.Expr.Native   => fail(V3ProjectionError.UnsupportedFeature(path, "Native"))
        case _: cm.Expr.External => fail(V3ProjectionError.UnsupportedFeature(path, "External"))
    }

  private def projectValueAttributes(expression: cm.Expr, path: String): Projection[Value] =
    val attributes = expression match
      case cm.Expr.Literal(value, _)             => value
      case cm.Expr.Constructor(value, _)         => value
      case cm.Expr.Tuple(value, _)               => value
      case cm.Expr.List(value, _)                => value
      case cm.Expr.Record(value, _)              => value
      case cm.Expr.Unit(value)                   => value
      case cm.Expr.Variable(value, _)            => value
      case cm.Expr.Reference(value, _)           => value
      case cm.Expr.Field(value, _, _)            => value
      case cm.Expr.FieldFunction(value, _)       => value
      case cm.Expr.Apply(value, _, _)            => value
      case cm.Expr.Lambda(value, _, _)           => value
      case cm.Expr.LetDefinition(value, _, _, _) => value
      case cm.Expr.LetRecursion(value, _, _)     => value
      case cm.Expr.Destructure(value, _, _, _)   => value
      case cm.Expr.IfThenElse(value, _, _, _)    => value
      case cm.Expr.PatternMatch(value, _, _)     => value
      case cm.Expr.UpdateRecord(value, _, _)     => value
      case cm.Expr.Hole(value, _, _)             => value
      case cm.Expr.Native(value, _, _)           => value
      case cm.Expr.External(value, _, _)         => value

    if attributes.source.nonEmpty || attributes.properties.nonEmpty || attributes.extensions.nonEmpty then
      fail(V3ProjectionError.UnsupportedFeature(path, "ValueAttributes"))
    else
      attributes.inferredType match
        case Some(inferredType) => projectType(inferredType, s"$path.inferredType")
        case None               => succeed(record())

  private def projectExpressions(expressions: Chunk[cm.Expr], path: String): Projection[Value] =
    mapResult(traverse(expressions.toList.zipWithIndex) { case (expression, index) =>
      projectExpr(expression, s"$path[$index]")
    })(values => sequence(values*))

  private def projectRecordFields(fields: Chunk[cm.RecordField], path: String): Projection[Value] =
    mapResult(traverse(fields.toList.zipWithIndex) { case (field, index) =>
      mapResult(projectExpr(field.value, s"$path[$index].value"))(projected =>
        sequence(name(field.name), projected)
      )
    })(values => sequence(values*))

  private def projectBindings(bindings: Chunk[cm.Binding], path: String): Projection[Value] =
    mapResult(traverse(bindings.toList.zipWithIndex) { case (binding, index) =>
      mapResult(projectValueDefinitionBody(binding.definition, s"$path[$index].definition"))(projected =>
        sequence(name(binding.name), projected)
      )
    })(values => sequence(values*))

  private def projectMatchCases(cases: Chunk[cm.MatchCase], path: String): Projection[Value] =
    mapResult(traverse(cases.toList.zipWithIndex) { case (matchCase, index) =>
      flatMapResult(projectPattern(matchCase.pattern, s"$path[$index].pattern")) { pattern =>
        mapResult(projectExpr(matchCase.body, s"$path[$index].body"))(body => sequence(pattern, body))
      }
    })(values => sequence(values*))

  private def projectPattern(pattern: cm.Pattern, path: String): Projection[Value] =
    flatMapResult(projectPatternAttributes(pattern, path)) { attributes =>
      pattern match
        case cm.Pattern.WildcardPattern(_)                => succeed(sequence(str("WildcardPattern"), attributes))
        case cm.Pattern.AsPattern(_, nested, patternName) =>
          mapResult(projectPattern(nested, s"$path.pattern"))(projected =>
            sequence(str("AsPattern"), attributes, projected, name(patternName))
          )
        case cm.Pattern.TuplePattern(_, elements) =>
          mapResult(projectPatterns(elements, s"$path.elements"))(projected =>
            sequence(str("TuplePattern"), attributes, projected)
          )
        case cm.Pattern.ConstructorPattern(_, constructor, args) =>
          mapResult(projectPatterns(args, s"$path.args"))(projected =>
            sequence(str("ConstructorPattern"), attributes, fqName(constructor), projected)
          )
        case cm.Pattern.EmptyListPattern(_)            => succeed(sequence(str("EmptyListPattern"), attributes))
        case cm.Pattern.HeadTailPattern(_, head, tail) =>
          flatMapResult(projectPattern(head, s"$path.head")) { projectedHead =>
            mapResult(projectPattern(tail, s"$path.tail"))(projectedTail =>
              sequence(str("HeadTailPattern"), attributes, projectedHead, projectedTail)
            )
          }
        case cm.Pattern.LiteralPattern(_, literal) =>
          mapResult(projectLiteral(literal, s"$path.literal"))(projected =>
            sequence(str("LiteralPattern"), attributes, projected)
          )
        case cm.Pattern.UnitPattern(_) => succeed(sequence(str("UnitPattern"), attributes))
    }

  private def projectPatternAttributes(pattern: cm.Pattern, path: String): Projection[Value] =
    val attributes = pattern match
      case cm.Pattern.WildcardPattern(value)          => value
      case cm.Pattern.AsPattern(value, _, _)          => value
      case cm.Pattern.TuplePattern(value, _)          => value
      case cm.Pattern.ConstructorPattern(value, _, _) => value
      case cm.Pattern.EmptyListPattern(value)         => value
      case cm.Pattern.HeadTailPattern(value, _, _)    => value
      case cm.Pattern.LiteralPattern(value, _)        => value
      case cm.Pattern.UnitPattern(value)              => value
    if attributes.source.nonEmpty || attributes.properties.nonEmpty || attributes.extensions.nonEmpty then
      fail(V3ProjectionError.UnsupportedFeature(path, "PatternAttributes"))
    else
      attributes.inferredType match
        case Some(inferredType) => projectType(inferredType, s"$path.inferredType")
        case None               => succeed(record())

  private def projectPatterns(patterns: Chunk[cm.Pattern], path: String): Projection[Value] =
    mapResult(traverse(patterns.toList.zipWithIndex) { case (pattern, index) =>
      projectPattern(pattern, s"$path[$index]")
    })(values => sequence(values*))

  private def projectLiteral(literal: cm.Literal, path: String): Projection[Value] =
    literal match
      case cm.Literal.BoolLiteral(value)                      => succeed(sequence(str("BoolLiteral"), bool(value)))
      case cm.Literal.CharLiteral(value) if value.length == 1 =>
        succeed(sequence(str("CharLiteral"), str(value)))
      case cm.Literal.CharLiteral(_)       => fail(V3ProjectionError.UnsupportedFeature(path, "non-BMP CharLiteral"))
      case cm.Literal.StringLiteral(value) => succeed(sequence(str("StringLiteral"), str(value)))
      case cm.Literal.IntegerLiteral(value) if value.isValidLong =>
        succeed(sequence(str("WholeNumberLiteral"), Structure.Value.Integer(value.longValue)))
      case cm.Literal.IntegerLiteral(_) =>
        fail(V3ProjectionError.UnsupportedFeature(path, "integer outside the v3 Long range"))
      case cm.Literal.FloatLiteral(value) if value.isFinite =>
        succeed(sequence(str("FloatLiteral"), Structure.Value.Decimal(value)))
      case cm.Literal.FloatLiteral(_) =>
        fail(V3ProjectionError.UnsupportedFeature(path, "non-finite FloatLiteral"))
      case cm.Literal.DecimalLiteral(value) => succeed(sequence(str("DecimalLiteral"), str(value.toString)))

  private def access(value: cm.Access): Value = str(value match
    case cm.Access.Public  => "Public"
    case cm.Access.Private => "Private")

  private def name(value: Name): Value                                = sequence(value.toList.map(str)*)
  private def namingPath(value: org.finos.morphir.naming.Path): Value =
    sequence(value.segments.map(name)*)
  private def packageName(value: PackageName): Value = namingPath(value.toPath)
  private def moduleName(value: ModuleName): Value   = namingPath(value.toPath)
  private def fqName(value: FQName): Value           =
    sequence(packageName(value.packagePath), moduleName(value.modulePath), name(value.localName))

  private def str(value: String): Value               = Structure.Value.Str(value)
  private def integer(value: Long): Value             = Structure.Value.Integer(value)
  private def bool(value: Boolean): Value             = Structure.Value.Bool(value)
  private def sequence(values: Value*): Value         = Structure.Value.Sequence(Chunk.from(values))
  private def record(fields: (String, Value)*): Value = Structure.Value.Record(Chunk.from(fields))

  private def succeed[A](value: A): Projection[A]              = Result.succeed(value)
  private def fail[A](error: V3ProjectionError): Projection[A] = Result.fail(error)

  private def mapResult[A, B](result: Projection[A])(f: A => B): Projection[B] =
    result match
      case Result.Success(value) => succeed(f(value))
      case Result.Failure(error) => fail(error)
      case Result.Panic(cause)   => Result.panic(cause)

  private def flatMapResult[A, B](result: Projection[A])(f: A => Projection[B]): Projection[B] =
    result match
      case Result.Success(value) => f(value)
      case Result.Failure(error) => fail(error)
      case Result.Panic(cause)   => Result.panic(cause)

  private def traverse[A](values: List[A])(projectValue: A => Projection[Value]): Projection[List[Value]] =
    @tailrec
    def loop(remaining: List[A], reversed: List[Value]): Projection[List[Value]] = remaining match
      case Nil          => succeed(reversed.reverse)
      case head :: tail => projectValue(head) match
          case Result.Success(projected) => loop(tail, projected :: reversed)
          case Result.Failure(error)     => fail(error)
          case Result.Panic(cause)       => Result.panic(cause)

    loop(values, Nil)

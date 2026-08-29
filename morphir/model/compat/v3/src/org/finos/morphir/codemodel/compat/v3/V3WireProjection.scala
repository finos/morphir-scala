package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.Json.given_Json
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.control.TailCalls.{TailRec, done, tailcall}

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
    projectValueDefinitionBodyLoop(body, path).result

  private def projectValueDefinitionBodyLoop(
      body: cm.ValueDefinitionBody,
      path: String
  ): TailRec[Projection[Value]] =
    body match
      case cm.ValueDefinitionBody.ExpressionBody(inputTypes, outputType, expression) =>
        flatMapTail(projectParameters(inputTypes, s"$path.inputTypes", typed = true)) { inputs =>
          flatMapTail(projectType(outputType, s"$path.outputType")) { output =>
            tailcall(projectExprLoop(expression, s"$path.body")).map { projectedBody =>
              mapResult(projectedBody)(body =>
                record(
                  "inputTypes" -> inputs,
                  "outputType" -> output,
                  "body"       -> body
                )
              )
            }
          }
        }
      case _: cm.ValueDefinitionBody.NativeBody =>
        done(fail(V3ProjectionError.UnsupportedFeature(path, "NativeBody")))
      case _: cm.ValueDefinitionBody.ExternalBody =>
        done(fail(V3ProjectionError.UnsupportedFeature(path, "ExternalBody")))
      case _: cm.ValueDefinitionBody.IncompleteBody =>
        done(fail(V3ProjectionError.UnsupportedFeature(path, "IncompleteBody")))

  private def projectParameters(parameters: Chunk[cm.Parameter], path: String, typed: Boolean): Projection[Value] =
    mapResult(traverse(parameters.toList.zipWithIndex) { case (parameter, index) =>
      mapResult(projectType(parameter.tpe, s"$path[$index].type")) { projectedType =>
        if typed then sequence(name(parameter.name), projectedType, projectedType)
        else sequence(name(parameter.name), projectedType)
      }
    })(values => sequence(values*))

  private def projectType(tpe: cm.Type, path: String): Projection[Value] =
    projectTypeLoop(tpe, path).result

  private def projectTypeLoop(tpe: cm.Type, path: String): TailRec[Projection[Value]] =
    flatMapTail(projectTypeAttributes(tpe, path)) { attributes =>
      tpe match
        case cm.Type.Variable(_, variableName) =>
          done(succeed(sequence(str("Variable"), attributes, name(variableName))))
        case cm.Type.Reference(_, reference, args) =>
          traverseTail(args.toList.zipWithIndex) { case (argument, index) =>
            tailcall(projectTypeLoop(argument, s"$path.args[$index]"))
          }.map(result =>
            mapResult(result)(projected =>
              sequence(str("Reference"), attributes, fqName(reference), sequence(projected*))
            )
          )
        case cm.Type.Tuple(_, elements) =>
          traverseTail(elements.toList.zipWithIndex) { case (element, index) =>
            tailcall(projectTypeLoop(element, s"$path.elements[$index]"))
          }.map(result =>
            mapResult(result)(projected => sequence(str("Tuple"), attributes, sequence(projected*)))
          )
        case cm.Type.Record(_, fields) =>
          tailcall(projectTypeFieldsLoop(fields, s"$path.fields")).map(result =>
            mapResult(result)(projected => sequence(str("Record"), attributes, projected))
          )
        case cm.Type.ExtensibleRecord(_, variable, fields) =>
          tailcall(projectTypeFieldsLoop(fields, s"$path.fields")).map(result =>
            mapResult(result)(projected =>
              sequence(str("ExtensibleRecord"), attributes, name(variable), projected)
            )
          )
        case cm.Type.Function(_, argumentType, returnType) =>
          tailcall(projectTypeLoop(argumentType, s"$path.argument")).flatMap { argumentResult =>
            flatMapTail(argumentResult) { argument =>
              tailcall(projectTypeLoop(returnType, s"$path.return")).map(result =>
                mapResult(result)(projected => sequence(str("Function"), attributes, argument, projected))
              )
            }
          }
        case cm.Type.Unit(_) => done(succeed(sequence(str("Unit"), attributes)))
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

  private def projectTypeFieldsLoop(fields: Chunk[cm.Field], path: String): TailRec[Projection[Value]] =
    traverseTail(fields.toList.zipWithIndex) { case (field, index) =>
      tailcall(projectTypeLoop(field.fieldType, s"$path[$index].type")).map(result =>
        mapResult(result)(projectedType => record("name" -> name(field.name), "tpe" -> projectedType))
      )
    }.map(result => mapResult(result)(values => sequence(values*)))

  private def projectExprLoop(expression: cm.Expr, path: String): TailRec[Projection[Value]] =
    flatMapTail(projectValueAttributes(expression, path)) { attributes =>
      expression match
        case cm.Expr.Literal(_, literal) =>
          done(
            mapResult(projectLiteral(literal, s"$path.literal"))(value =>
              sequence(str("Literal"), attributes, value)
            )
          )
        case cm.Expr.Constructor(_, reference) =>
          done(succeed(sequence(str("Constructor"), attributes, fqName(reference))))
        case cm.Expr.Tuple(_, elements) =>
          tailcall(projectExpressionsLoop(elements, s"$path.elements")).map(result =>
            mapResult(result)(values => sequence(str("Tuple"), attributes, values))
          )
        case cm.Expr.List(_, items) =>
          tailcall(projectExpressionsLoop(items, s"$path.items")).map(result =>
            mapResult(result)(values => sequence(str("List"), attributes, values))
          )
        case cm.Expr.Record(_, fields) =>
          tailcall(projectRecordFieldsLoop(fields, s"$path.fields")).map(result =>
            mapResult(result)(values => sequence(str("Record"), attributes, values))
          )
        case cm.Expr.Unit(_)                   => done(succeed(sequence(str("Unit"), attributes)))
        case cm.Expr.Variable(_, variableName) =>
          done(succeed(sequence(str("Variable"), attributes, name(variableName))))
        case cm.Expr.Reference(_, reference) =>
          done(succeed(sequence(str("Reference"), attributes, fqName(reference))))
        case cm.Expr.Field(_, subject, fieldName) =>
          tailcall(projectExprLoop(subject, s"$path.record")).map(result =>
            mapResult(result)(projected => sequence(str("Field"), attributes, projected, name(fieldName)))
          )
        case cm.Expr.FieldFunction(_, fieldName) =>
          done(succeed(sequence(str("FieldFunction"), attributes, name(fieldName))))
        case cm.Expr.Apply(_, function, argument) =>
          tailcall(projectExprLoop(function, s"$path.function")).flatMap { functionResult =>
            flatMapTail(functionResult) { projectedFunction =>
              tailcall(projectExprLoop(argument, s"$path.argument")).map { argumentResult =>
                mapResult(argumentResult)(projectedArgument =>
                  sequence(str("Apply"), attributes, projectedFunction, projectedArgument)
                )
              }
            }
          }
        case cm.Expr.Lambda(_, argumentPattern, body) =>
          flatMapTail(projectPattern(argumentPattern, s"$path.argumentPattern")) { pattern =>
            tailcall(projectExprLoop(body, s"$path.body")).map(result =>
              mapResult(result)(projectedBody => sequence(str("Lambda"), attributes, pattern, projectedBody))
            )
          }
        case cm.Expr.LetDefinition(_, valueName, definition, inValue) =>
          tailcall(projectValueDefinitionBodyLoop(definition, s"$path.definition")).flatMap { definitionResult =>
            flatMapTail(definitionResult) { projectedDefinition =>
              tailcall(projectExprLoop(inValue, s"$path.inValue")).map(result =>
                mapResult(result)(projectedInValue =>
                  sequence(str("LetDefinition"), attributes, name(valueName), projectedDefinition, projectedInValue)
                )
              )
            }
          }
        case cm.Expr.LetRecursion(_, bindings, inValue) =>
          tailcall(projectBindingsLoop(bindings, s"$path.bindings")).flatMap { bindingsResult =>
            flatMapTail(bindingsResult) { projectedBindings =>
              tailcall(projectExprLoop(inValue, s"$path.inValue")).map(result =>
                mapResult(result)(projectedInValue =>
                  sequence(str("LetRecursion"), attributes, projectedBindings, projectedInValue)
                )
              )
            }
          }
        case cm.Expr.Destructure(_, pattern, valueToDestructure, inValue) =>
          flatMapTail(projectPattern(pattern, s"$path.pattern")) { projectedPattern =>
            tailcall(projectExprLoop(valueToDestructure, s"$path.value")).flatMap { valueResult =>
              flatMapTail(valueResult) { projectedValue =>
                tailcall(projectExprLoop(inValue, s"$path.inValue")).map(result =>
                  mapResult(result)(projectedInValue =>
                    sequence(str("Destructure"), attributes, projectedPattern, projectedValue, projectedInValue)
                  )
                )
              }
            }
          }
        case cm.Expr.IfThenElse(_, condition, thenBranch, elseBranch) =>
          tailcall(projectExprLoop(condition, s"$path.condition")).flatMap { conditionResult =>
            flatMapTail(conditionResult) { projectedCondition =>
              tailcall(projectExprLoop(thenBranch, s"$path.then")).flatMap { thenResult =>
                flatMapTail(thenResult) { projectedThen =>
                  tailcall(projectExprLoop(elseBranch, s"$path.else")).map(result =>
                    mapResult(result)(projectedElse =>
                      sequence(str("IfThenElse"), attributes, projectedCondition, projectedThen, projectedElse)
                    )
                  )
                }
              }
            }
          }
        case cm.Expr.PatternMatch(_, subject, cases) =>
          tailcall(projectExprLoop(subject, s"$path.subject")).flatMap { subjectResult =>
            flatMapTail(subjectResult) { projectedSubject =>
              tailcall(projectMatchCasesLoop(cases, s"$path.cases")).map(result =>
                mapResult(result)(projectedCases =>
                  sequence(str("PatternMatch"), attributes, projectedSubject, projectedCases)
                )
              )
            }
          }
        case cm.Expr.UpdateRecord(_, recordValue, updates) =>
          tailcall(projectExprLoop(recordValue, s"$path.record")).flatMap { recordResult =>
            flatMapTail(recordResult) { projectedRecord =>
              tailcall(projectRecordFieldsLoop(updates, s"$path.updates")).map(result =>
                mapResult(result)(projectedUpdates =>
                  sequence(str("UpdateRecord"), attributes, projectedRecord, projectedUpdates)
                )
              )
            }
          }
        case _: cm.Expr.Hole     => done(fail(V3ProjectionError.UnsupportedFeature(path, "Hole")))
        case _: cm.Expr.Native   => done(fail(V3ProjectionError.UnsupportedFeature(path, "Native")))
        case _: cm.Expr.External => done(fail(V3ProjectionError.UnsupportedFeature(path, "External")))
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

  private def projectExpressionsLoop(
      expressions: Chunk[cm.Expr],
      path: String
  ): TailRec[Projection[Value]] =
    traverseTail(expressions.toList.zipWithIndex) { case (expression, index) =>
      tailcall(projectExprLoop(expression, s"$path[$index]"))
    }.map(result => mapResult(result)(values => sequence(values*)))

  private def projectRecordFieldsLoop(
      fields: Chunk[cm.RecordField],
      path: String
  ): TailRec[Projection[Value]] =
    traverseTail(fields.toList.zipWithIndex) { case (field, index) =>
      tailcall(projectExprLoop(field.value, s"$path[$index].value")).map(result =>
        mapResult(result)(projected => sequence(name(field.name), projected))
      )
    }.map(result => mapResult(result)(values => sequence(values*)))

  private def projectBindingsLoop(
      bindings: Chunk[cm.Binding],
      path: String
  ): TailRec[Projection[Value]] =
    traverseTail(bindings.toList.zipWithIndex) { case (binding, index) =>
      tailcall(projectValueDefinitionBodyLoop(binding.definition, s"$path[$index].definition")).map(result =>
        mapResult(result)(projected => sequence(name(binding.name), projected))
      )
    }.map(result => mapResult(result)(values => sequence(values*)))

  private def projectMatchCasesLoop(
      cases: Chunk[cm.MatchCase],
      path: String
  ): TailRec[Projection[Value]] =
    traverseTail(cases.toList.zipWithIndex) { case (matchCase, index) =>
      flatMapTail(projectPattern(matchCase.pattern, s"$path[$index].pattern")) { pattern =>
        tailcall(projectExprLoop(matchCase.body, s"$path[$index].body")).map(result =>
          mapResult(result)(body => sequence(pattern, body))
        )
      }
    }.map(result => mapResult(result)(values => sequence(values*)))

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
      case cm.Literal.BoolLiteral(value) => succeed(sequence(str("BoolLiteral"), bool(value)))
      case cm.Literal.CharLiteral(value) if value.codePointCount(0, value.length) == 1 =>
        succeed(sequence(str("CharLiteral"), str(value)))
      case cm.Literal.CharLiteral(_) =>
        fail(V3ProjectionError.UnsupportedFeature(path, "CharLiteral with code point count other than one"))
      case cm.Literal.StringLiteral(value)                       => succeed(sequence(str("StringLiteral"), str(value)))
      case cm.Literal.IntegerLiteral(value) if value.isValidLong =>
        succeed(sequence(str("WholeNumberLiteral"), Structure.Value.Integer(value.longValue)))
      case cm.Literal.IntegerLiteral(_) =>
        fail(V3ProjectionError.UnsupportedFeature(path, "integer outside the v3 Long range"))
      case cm.Literal.FloatLiteral(value) if value.isFinite =>
        succeed(sequence(str("FloatLiteral"), Structure.Value.Decimal(value)))
      case cm.Literal.FloatLiteral(_) =>
        fail(V3ProjectionError.UnsupportedFeature(path, "non-finite FloatLiteral"))
      case cm.Literal.DecimalLiteral(value) =>
        succeed(sequence(str("DecimalLiteral"), str(value.bigDecimal.toPlainString)))

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

  private def flatMapTail[A, B](
      result: Projection[A]
  )(f: A => TailRec[Projection[B]]): TailRec[Projection[B]] = result match
    case Result.Success(value) => f(value)
    case Result.Failure(error) => done(fail(error))
    case Result.Panic(cause)   => done(Result.panic(cause))

  private def traverseTail[A](
      values: List[A]
  )(projectValue: A => TailRec[Projection[Value]]): TailRec[Projection[List[Value]]] =
    def loop(remaining: List[A], reversed: List[Value]): TailRec[Projection[List[Value]]] = remaining match
      case Nil          => done(succeed(reversed.reverse))
      case head :: tail =>
        tailcall(projectValue(head)).flatMap {
          case Result.Success(projected) => tailcall(loop(tail, projected :: reversed))
          case Result.Failure(error)     => done(fail(error))
          case Result.Panic(cause)       => done(Result.panic(cause))
        }

    loop(values, Nil)

  private def traverse[A](values: List[A])(projectValue: A => Projection[Value]): Projection[List[Value]] =
    @tailrec
    def loop(remaining: List[A], reversed: List[Value]): Projection[List[Value]] = remaining match
      case Nil          => succeed(reversed.reverse)
      case head :: tail => projectValue(head) match
          case Result.Success(projected) => loop(tail, projected :: reversed)
          case Result.Failure(error)     => fail(error)
          case Result.Panic(cause)       => Result.panic(cause)

    loop(values, Nil)

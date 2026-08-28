package morphir.langkit.elm.compiler.ir

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast
import morphir.langkit.elm.compiler.ParseDiagnostic
import org.finos.morphir.ir.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.naming.*

final case class CompileInput(
    source: String,
    packageName: PackageName,
    moduleName: ModuleName,
    exposedValues: Set[Name],
    irVersion: MorphirIRVersion
) derives CanEqual

final case class CompileFailure(diagnostics: Chunk[CompileDiagnostic]) derives CanEqual

enum CompileDiagnostic derives CanEqual:
  case ParserFailure(diagnostic: ParseDiagnostic)
  case MalformedModuleHeader(span: Span)
  case UnsupportedModule(moduleType: ast.ModuleType, span: Span)
  case UnsupportedImport(moduleName: String, span: Span)
  case UnsupportedDeclaration(kind: String, span: Span)
  case UnsupportedType(kind: String, span: Span)
  case UnsupportedExpression(kind: String, span: Span)
  case UnsupportedPattern(kind: String, span: Span)
  case ModuleNameMismatch(expected: ModuleName, actual: ModuleName, span: Span)
  case ExposedNameMismatch(expected: Set[Name], actual: Set[Name], span: Span)
  case UnsupportedIRVersion(version: MorphirIRVersion)
  case UnsupportedExposure(kind: String, span: Span)

  def code: String = this match
    case ParserFailure(_)             => "ELM-IR001"
    case MalformedModuleHeader(_)     => "ELM-IR002"
    case UnsupportedModule(_, _)      => "ELM-IR003"
    case UnsupportedImport(_, _)      => "ELM-IR004"
    case UnsupportedDeclaration(_, _) => "ELM-IR005"
    case UnsupportedType(_, _)        => "ELM-IR006"
    case UnsupportedExpression(_, _)  => "ELM-IR007"
    case UnsupportedPattern(_, _)     => "ELM-IR008"
    case ModuleNameMismatch(_, _, _)  => "ELM-IR009"
    case ExposedNameMismatch(_, _, _) => "ELM-IR010"
    case UnsupportedIRVersion(_)      => "ELM-IR011"
    case UnsupportedExposure(_, _)    => "ELM-IR012"

object ElmToMorphirIRCompiler:

  def compile(input: CompileInput): Result[CompileFailure, MorphirIRFile] =
    if input.irVersion != MorphirIRVersion.V3_0 then
      Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedIRVersion(input.irVersion))))
    else
      Elm.parseAst(input.source).fold(
        diagnostic =>
          malformedModuleHeaderSpan(input.source, diagnostic) match
            case Some(span) =>
              Result.fail(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span))))
            case None =>
              Result.fail(CompileFailure(Chunk(CompileDiagnostic.ParserFailure(diagnostic)))),
        module =>
          if module.moduleType != ast.ModuleType.Plain then
            Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedModule(module.moduleType, module.span))))
          else if ModuleName.fromStrings(module.name.parts*) != input.moduleName then
            Result.fail(
              CompileFailure(
                Chunk(
                  CompileDiagnostic.ModuleNameMismatch(
                    input.moduleName,
                    ModuleName.fromStrings(module.name.parts*),
                    module.name.span
                  )
                )
              )
            )
          else if unsupportedExposure(module).nonEmpty then
            val (kind, span) = unsupportedExposure(module).get
            Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedExposure(kind, span))))
          else if exposedValueNames(module) != input.exposedValues then
            Result.fail(
              CompileFailure(
                Chunk(
                  CompileDiagnostic.ExposedNameMismatch(
                    input.exposedValues,
                    exposedValueNames(module),
                    module.exposing.span
                  )
                )
              )
            )
          else if module.imports.nonEmpty then
            val unsupported = module.imports.head
            Result.fail(
              CompileFailure(
                Chunk(CompileDiagnostic.UnsupportedImport(unsupported.moduleName.fullName, unsupported.span))
              )
            )
          else if module.declarations.exists(!_.isInstanceOf[ast.ValueDeclaration]) then
            val unsupported = module.declarations.find(!_.isInstanceOf[ast.ValueDeclaration]).get
            val kind        = unsupported match
              case _: ast.TypeAliasDeclaration  => "type alias"
              case _: ast.CustomTypeDeclaration => "custom type"
              case _: ast.PortDeclaration       => "port"
              case _: ast.InfixDeclaration      => "infix"
              case _: ast.ValueDeclaration      => "value"
            Result.fail(
              CompileFailure(Chunk(CompileDiagnostic.UnsupportedDeclaration(kind, unsupported.span)))
            )
          else if module.declarations.size != 1 then
            Result.fail(
              CompileFailure(
                Chunk(
                  CompileDiagnostic.UnsupportedDeclaration(
                    s"module with ${module.declarations.size} value declarations",
                    module.span
                  )
                )
              )
            )
          else if declaredValueNames(module) != input.exposedValues then
            Result.fail(
              CompileFailure(
                Chunk(
                  CompileDiagnostic.ExposedNameMismatch(
                    input.exposedValues,
                    declaredValueNames(module),
                    module.declarations.head.span
                  )
                )
              )
            )
          else
            module.declarations.collectFirst(Function.unlift {
              case declaration: ast.ValueDeclaration => unsupportedType(declaration)
              case _                                 => None
            }) match
              case Some((kind, span)) =>
                Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedType(kind, span))))
              case None =>
                module.declarations.collectFirst(Function.unlift {
                  case declaration: ast.ValueDeclaration => unsupportedPattern(declaration.parameters)
                  case _                                 => None
                }) match
                  case Some((kind, span)) =>
                    Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedPattern(kind, span))))
                  case None => compileExpressions(input, module)
      )

  private def compileExpressions(
      input: CompileInput,
      module: ast.Module
  ): Result[CompileFailure, MorphirIRFile] =
    module.declarations.collectFirst(Function.unlift {
      case declaration: ast.ValueDeclaration => unsupportedExpression(declaration)
      case _                                 => None
    }) match
      case Some((kind, span)) =>
        Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedExpression(kind, span))))
      case None => Result.succeed(lower(input, module))

  private def malformedModuleHeaderSpan(source: String, diagnostic: ParseDiagnostic): Option[Span] =
    val start = source.indexWhere(!_.isWhitespace)
    if start < 0 then None
    else
      val end = source.indexOf('\n', start) match
        case -1    => source.length
        case index => index
      val firstLine        = source.substring(start, end)
      val firstLineNumber  = source.substring(0, start).count(_ == '\n') + 1
      val beginsLikeHeader =
        firstLine.startsWith("module") || firstLine.startsWith("port module") || firstLine.startsWith("effect module")
      Option.when(beginsLikeHeader && diagnostic.span.line == firstLineNumber)(Span.fromStartEnd(start, end))

  private def exposedValueNames(module: ast.Module): Set[Name] =
    module.exposing match
      case ast.ExposingExplicit(items) =>
        items.collect { case ast.ExposedValue(name) => Name.fromString(name) }.toSet
      case _: ast.ExposingAll =>
        module.declarations.collect { case declaration: ast.ValueDeclaration =>
          Name.fromString(declaration.name)
        }.toSet

  private def unsupportedExposure(module: ast.Module): Option[(String, Span)] =
    module.exposing match
      case ast.ExposingExplicit(items) =>
        items.collectFirst {
          case operator @ ast.ExposedOperator(name)   => s"operator $name" -> operator.span
          case exposedType @ ast.ExposedType(name, _) => s"type $name"     -> exposedType.span
        }
      case exposeAll: ast.ExposingAll => Some("all values" -> exposeAll.span)

  private def declaredValueNames(module: ast.Module): Set[Name] =
    module.declarations.collect { case declaration: ast.ValueDeclaration =>
      Name.fromString(declaration.name)
    }.toSet

  private def unsupportedType(declaration: ast.ValueDeclaration): Option[(String, Span)] =
    declaration.typeAnnotation match
      case None      => Some("missing type annotation" -> declaration.span)
      case Some(tpe) =>
        unsupportedType(tpe).orElse {
          tpe match
            case ast.FunctionType(
                  ast.TypeReference(first),
                  ast.FunctionType(ast.TypeReference(second), ast.TypeReference(output))
                ) if List(first, second, output).forall(_.parts == List("Int")) => None
            case _ => Some("function signature" -> tpe.span)
        }

  private def unsupportedType(tpe: ast.TypeExpression): Option[(String, Span)] =
    tpe match
      case reference @ ast.TypeReference(name) =>
        if name.parts == List("Int") then None else Some(name.fullName -> reference.span)
      case ast.FunctionType(from, to) =>
        unsupportedType(from).orElse(unsupportedType(to))
      case other => Some(typeKind(other) -> other.span)

  private def typeKind(tpe: ast.TypeExpression): String =
    tpe match
      case _: ast.TypeVariable    => "type variable"
      case _: ast.TypeApplication => "type application"
      case _: ast.TupleType       => "tuple"
      case _: ast.UnitType        => "unit"
      case _: ast.RecordType      => "record"
      case _: ast.TypeReference   => "reference"
      case _: ast.FunctionType    => "function"

  private def unsupportedExpression(declaration: ast.ValueDeclaration): Option[(String, Span)] =
    declaration.body match
      case binary @ ast.BinaryOp(ast.VariableRef(left), "+", ast.VariableRef(right))
          if left.parts.size == 1 && right.parts.size == 1 =>
        val parameters = declaration.parameters.collect { case ast.VariablePattern(name) => name }
        if parameters == IndexedSeq(left.parts.head, right.parts.head) then None
        else Some("variables do not match parameters" -> binary.span)
      case binary @ ast.BinaryOp(_, operator, _) => Some(s"binary operator $operator" -> binary.span)
      case other                                 => Some(expressionKind(other) -> other.span)

  private def unsupportedPattern(parameters: IndexedSeq[ast.Pattern]): Option[(String, Span)] =
    parameters.collectFirst {
      case pattern if !pattern.isInstanceOf[ast.VariablePattern] => patternKind(pattern) -> pattern.span
    }

  private def patternKind(pattern: ast.Pattern): String =
    pattern match
      case _: ast.AnythingPattern    => "anything"
      case _: ast.IntPattern         => "integer"
      case _: ast.FloatPattern       => "float"
      case _: ast.StringPattern      => "string"
      case _: ast.CharPattern        => "character"
      case _: ast.VariablePattern    => "variable"
      case _: ast.UnitPattern        => "unit"
      case _: ast.ConstructorPattern => "constructor"
      case _: ast.TuplePattern       => "tuple"
      case _: ast.ListPattern        => "list"
      case _: ast.ConsPattern        => "cons"
      case _: ast.RecordPattern      => "record"
      case _: ast.AsPattern          => "as"

  private def expressionKind(expression: ast.Expression): String =
    expression match
      case _: ast.IntLiteral          => "integer literal"
      case _: ast.FloatLiteral        => "float literal"
      case _: ast.StringLiteral       => "string literal"
      case _: ast.CharLiteral         => "character literal"
      case _: ast.VariableRef         => "variable reference"
      case _: ast.ConstructorRef      => "constructor reference"
      case _: ast.OperatorRef         => "operator reference"
      case _: ast.FunctionApplication => "function application"
      case _: ast.BinaryOp            => "binary operation"
      case _: ast.Negate              => "negation"
      case _: ast.IfThenElse          => "if expression"
      case _: ast.LetIn               => "let expression"
      case _: ast.CaseOf              => "case expression"
      case _: ast.Lambda              => "lambda"
      case _: ast.TupleLiteral        => "tuple literal"
      case _: ast.UnitLiteral         => "unit literal"
      case _: ast.ListLiteral         => "list literal"
      case _: ast.RecordLiteral       => "record literal"
      case _: ast.RecordUpdate        => "record update"
      case _: ast.FieldAccess         => "field access"
      case _: ast.FieldAccessFunction => "field access function"
      case _: ast.Parenthesized       => "parenthesized expression"
      case _: ast.Glsl                => "GLSL expression"

  private def lower(input: CompileInput, module: ast.Module): MorphirIRFile =
    val declaration    = module.declarations.head.asInstanceOf[ast.ValueDeclaration]
    val parameterNames = declaration.parameters.collect { case ast.VariablePattern(name) => name }
    val addDefinition  = Value.Definition.Typed(
      parameterNames.map(_ -> Basics.intType)*
    )(Basics.intType) {
      Value.applyInferType(
        Basics.intType,
        Basics.add,
        parameterNames.map(Value.variable(_, Basics.intType))*
      )
    }
    val moduleDefinition = Module.Definition(
      types = Map.empty,
      values = Map(
        Name.fromString(declaration.name) -> AccessControlled.publicAccess(Documented("", addDefinition))
      )
    )
    val packageDefinition = PackageModule.Definition.Typed(
      Map(input.moduleName -> AccessControlled.publicAccess(moduleDefinition))
    )
    MorphirIRFile(
      input.irVersion,
      Distribution.Library(input.packageName, dependencies = Map.empty, packageDefinition)
    )

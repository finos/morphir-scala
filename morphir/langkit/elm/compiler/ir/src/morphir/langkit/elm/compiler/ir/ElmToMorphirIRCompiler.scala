package morphir.langkit.elm.compiler.ir

import kyo.*
import morphir.langkit.core.Span
import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.{CstModule, CstPattern, CstValueDeclaration, CstVariablePattern}
import morphir.langkit.elm.parser.CstLowering
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
  case AnnotationNameMismatch(annotationName: String, declarationName: String, span: Span)
  case DuplicateParameter(name: String, span: Span)
  case DuplicateExposedValue(name: Name, span: Span)

  def code: String = this match
    case ParserFailure(_)                => "ELM-IR001"
    case MalformedModuleHeader(_)        => "ELM-IR002"
    case UnsupportedModule(_, _)         => "ELM-IR003"
    case UnsupportedImport(_, _)         => "ELM-IR004"
    case UnsupportedDeclaration(_, _)    => "ELM-IR005"
    case UnsupportedType(_, _)           => "ELM-IR006"
    case UnsupportedExpression(_, _)     => "ELM-IR007"
    case UnsupportedPattern(_, _)        => "ELM-IR008"
    case ModuleNameMismatch(_, _, _)     => "ELM-IR009"
    case ExposedNameMismatch(_, _, _)    => "ELM-IR010"
    case UnsupportedIRVersion(_)         => "ELM-IR011"
    case UnsupportedExposure(_, _)       => "ELM-IR012"
    case AnnotationNameMismatch(_, _, _) => "ELM-IR013"
    case DuplicateParameter(_, _)        => "ELM-IR014"
    case DuplicateExposedValue(_, _)     => "ELM-IR015"

object ElmToMorphirIRCompiler:

  def compile(input: CompileInput): Result[CompileFailure, MorphirIRFile] =
    if input.irVersion != MorphirIRVersion.V3_0 then
      Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedIRVersion(input.irVersion))))
    else
      Elm.parseCst(input.source).fold(
        diagnostic =>
          malformedModuleHeaderSpan(input.source, diagnostic) match
            case Some(span) =>
              Result.fail(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span))))
            case None =>
              Result.fail(CompileFailure(Chunk(CompileDiagnostic.ParserFailure(diagnostic)))),
        cst =>
          annotationNameMismatch(cst) match
            case Some((annotationName, declarationName, span)) =>
              Result.fail(
                CompileFailure(
                  Chunk(CompileDiagnostic.AnnotationNameMismatch(annotationName, declarationName, span))
                )
              )
            case None =>
              duplicateParameter(cst) match
                case Some((name, span)) =>
                  Result.fail(CompileFailure(Chunk(CompileDiagnostic.DuplicateParameter(name, span))))
                case None => compileModule(input, CstLowering.lowerModule(cst))
      )

  private def compileModule(input: CompileInput, module: ast.Module): Result[CompileFailure, MorphirIRFile] =
    validateModule(input, module) match
      case Some(diagnostic) => Result.fail(CompileFailure(Chunk(diagnostic)))
      case None             =>
        singleValueDeclaration(module) match
          case Left(diagnostic)   => Result.fail(CompileFailure(Chunk(diagnostic)))
          case Right(declaration) => compileDeclaration(input, module, declaration)

  private def validateModule(input: CompileInput, module: ast.Module): Option[CompileDiagnostic] =
    val actualModuleName = ModuleName.fromStrings(module.name.parts*)
    if module.moduleType != ast.ModuleType.Plain then
      Some(CompileDiagnostic.UnsupportedModule(module.moduleType, module.span))
    else if actualModuleName != input.moduleName then
      Some(CompileDiagnostic.ModuleNameMismatch(input.moduleName, actualModuleName, module.name.span))
    else
      duplicateExposedValue(module)
        .map((name, span) => CompileDiagnostic.DuplicateExposedValue(name, span))
        .orElse(
          unsupportedExposure(module).map((kind, span) => CompileDiagnostic.UnsupportedExposure(kind, span))
        )
        .orElse {
          val actualExposedValues = exposedValueNames(module)
          Option.when(actualExposedValues != input.exposedValues)(
            CompileDiagnostic.ExposedNameMismatch(input.exposedValues, actualExposedValues, module.exposing.span)
          )
        }
        .orElse {
          module.imports.headOption.map(unsupported =>
            CompileDiagnostic.UnsupportedImport(unsupported.moduleName.fullName, unsupported.span)
          )
        }

  private def singleValueDeclaration(
      module: ast.Module
  ): Either[CompileDiagnostic, ast.ValueDeclaration] =
    module.declarations.toList match
      case List(declaration: ast.ValueDeclaration) => Right(declaration)
      case declarations                            =>
        declarations.collectFirst {
          case unsupported: ast.TypeAliasDeclaration =>
            CompileDiagnostic.UnsupportedDeclaration("type alias", unsupported.span)
          case unsupported: ast.CustomTypeDeclaration =>
            CompileDiagnostic.UnsupportedDeclaration("custom type", unsupported.span)
          case unsupported: ast.PortDeclaration =>
            CompileDiagnostic.UnsupportedDeclaration("port", unsupported.span)
          case unsupported: ast.InfixDeclaration =>
            CompileDiagnostic.UnsupportedDeclaration("infix", unsupported.span)
        } match
          case Some(diagnostic) => Left(diagnostic)
          case None             =>
            Left(
              CompileDiagnostic.UnsupportedDeclaration(
                s"module with ${module.declarations.size} value declarations",
                module.span
              )
            )

  private def compileDeclaration(
      input: CompileInput,
      module: ast.Module,
      declaration: ast.ValueDeclaration
  ): Result[CompileFailure, MorphirIRFile] =
    val declaredValues = Set(Name.fromString(declaration.name))
    if declaredValues != input.exposedValues then
      Result.fail(
        CompileFailure(
          Chunk(
            CompileDiagnostic.ExposedNameMismatch(input.exposedValues, declaredValues, declaration.span)
          )
        )
      )
    else
      unsupportedType(declaration) match
        case Some((kind, span)) =>
          Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedType(kind, span))))
        case None =>
          unsupportedPattern(declaration.parameters) match
            case Some((kind, span)) =>
              Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedPattern(kind, span))))
            case None => compileExpression(input, module, declaration)

  private def annotationNameMismatch(cst: CstModule): Option[(String, String, Span)] =
    cst.declarations.collectFirst(Function.unlift {
      case declaration: CstValueDeclaration =>
        declaration.annotation.collect {
          case annotation if annotation.name.value != declaration.name.value =>
            (annotation.name.value, declaration.name.value, annotation.name.span)
        }
      case _ => None
    })

  private def duplicateParameter(cst: CstModule): Option[(String, Span)] =
    cst.declarations.collectFirst(Function.unlift {
      case declaration: CstValueDeclaration => duplicateVariableParameter(declaration.patterns.toList, Set.empty)
      case _                                => None
    })

  private def duplicateVariableParameter(
      remaining: List[CstPattern],
      seen: Set[String]
  ): Option[(String, Span)] =
    remaining match
      case (pattern: CstVariablePattern) :: _ if seen.contains(pattern.name.value) =>
        Some(pattern.name.value -> pattern.name.span)
      case (pattern: CstVariablePattern) :: tail =>
        duplicateVariableParameter(tail, seen + pattern.name.value)
      case _ :: tail => duplicateVariableParameter(tail, seen)
      case Nil       => None

  private def compileExpression(
      input: CompileInput,
      module: ast.Module,
      declaration: ast.ValueDeclaration
  ): Result[CompileFailure, MorphirIRFile] =
    unsupportedExpression(declaration) match
      case Some((kind, span)) =>
        Result.fail(CompileFailure(Chunk(CompileDiagnostic.UnsupportedExpression(kind, span))))
      case None => Result.succeed(lower(input, module, declaration))

  private def malformedModuleHeaderSpan(source: String, diagnostic: ParseDiagnostic): Option[Span] =
    val start = skipTrivia(source, 0, source.length)
    if start < 0 then None
    else
      val regionEnd = headerRegionEnd(source, start)
      if !beginsWithModuleHeaderTokens(source, start) then None
      else
        exposingListEnd(source, start, regionEnd) match
          case Some(headerEnd) =>
            Option.when(diagnostic.span.range.start < headerEnd)(Span.fromStartEnd(start, headerEnd))
          case None => Some(Span.fromStartEnd(start, regionEnd))

  private def beginsWithModuleHeaderTokens(source: String, start: Int): Boolean =
    readIdentifier(source, start) match
      case Some(("module", _))                      => true
      case Some(("port" | "effect", afterModifier)) =>
        val moduleStart = skipTrivia(source, afterModifier, source.length)
        moduleStart >= 0 && readIdentifier(source, moduleStart).exists(_._1 == "module")
      case _ => false

  private def exposingListEnd(source: String, start: Int, limit: Int): Option[Int] =
    findIdentifier(source, "exposing", start, limit).flatMap { afterExposing =>
      val open = skipTrivia(source, afterExposing, limit)
      Option.when(open >= 0 && open < limit && source(open) == '(')(open).flatMap { openParenthesis =>
        matchingParenthesisEnd(source, openParenthesis + 1, limit, depth = 1)
      }
    }

  private def findIdentifier(source: String, expected: String, offset: Int, limit: Int): Option[Int] =
    val start = skipTrivia(source, offset, limit)
    if start < 0 then None
    else
      readIdentifier(source, start) match
        case Some((identifier, end)) if identifier == expected => Some(end)
        case Some((_, end))                                    => findIdentifier(source, expected, end, limit)
        case None                                              => findIdentifier(source, expected, start + 1, limit)

  private def matchingParenthesisEnd(source: String, offset: Int, limit: Int, depth: Int): Option[Int] =
    if offset >= limit then None
    else
      val next = skipTrivia(source, offset, limit)
      if next < 0 then None
      else
        source(next) match
          case '('               => matchingParenthesisEnd(source, next + 1, limit, depth + 1)
          case ')' if depth == 1 => Some(next + 1)
          case ')'               => matchingParenthesisEnd(source, next + 1, limit, depth - 1)
          case _                 => matchingParenthesisEnd(source, next + 1, limit, depth)

  private def headerRegionEnd(source: String, start: Int): Int =
    def loop(lineStart: Int): Int =
      val lineEnd = source.indexOf('\n', lineStart) match
        case -1    => source.length
        case index => index
      if lineStart > start && source.substring(lineStart, lineEnd).forall(_.isWhitespace) then
        math.max(start, lineStart - 1)
      else if lineEnd == source.length then source.length
      else loop(lineEnd + 1)

    loop(start)

  private def readIdentifier(source: String, start: Int): Option[(String, Int)] =
    if start >= source.length || !source(start).isLetter then None
    else
      val end = source.indexWhere(character => !character.isLetterOrDigit && character != '_', start) match
        case -1    => source.length
        case index => index
      Some(source.substring(start, end) -> end)

  private def skipTrivia(source: String, offset: Int, limit: Int): Int =
    val nonWhitespace = source.indexWhere(!_.isWhitespace, offset)
    if nonWhitespace < 0 || nonWhitespace >= limit then -1
    else if source.startsWith("--", nonWhitespace) then
      source.indexOf('\n', nonWhitespace) match
        case -1      => -1
        case lineEnd => skipTrivia(source, lineEnd + 1, limit)
    else if source.startsWith("{-", nonWhitespace) then
      blockCommentEnd(source, nonWhitespace + 2, limit, depth = 1) match
        case Some(commentEnd) => skipTrivia(source, commentEnd, limit)
        case None             => -1
    else nonWhitespace

  private def blockCommentEnd(source: String, offset: Int, limit: Int, depth: Int): Option[Int] =
    if offset >= limit then None
    else if source.startsWith("{-", offset) then blockCommentEnd(source, offset + 2, limit, depth + 1)
    else if source.startsWith("-}", offset) then
      if depth == 1 then Some(offset + 2)
      else blockCommentEnd(source, offset + 2, limit, depth - 1)
    else blockCommentEnd(source, offset + 1, limit, depth)

  private def exposedValueNames(module: ast.Module): Set[Name] =
    module.exposing match
      case ast.ExposingExplicit(items) =>
        items.collect { case ast.ExposedValue(name) => Name.fromString(name) }.toSet
      case _: ast.ExposingAll =>
        module.declarations.collect { case declaration: ast.ValueDeclaration =>
          Name.fromString(declaration.name)
        }.toSet

  private def duplicateExposedValue(module: ast.Module): Option[(Name, Span)] =
    module.exposing match
      case ast.ExposingExplicit(items) => duplicateExposedValue(items.toList, Set.empty)
      case _: ast.ExposingAll          => None

  private def duplicateExposedValue(
      remaining: List[ast.ExposedItem],
      seen: Set[Name]
  ): Option[(Name, Span)] =
    remaining match
      case (item @ ast.ExposedValue(value)) :: _ if seen.contains(Name.fromString(value)) =>
        Some(Name.fromString(value) -> item.span)
      case ast.ExposedValue(value) :: tail =>
        duplicateExposedValue(tail, seen + Name.fromString(value))
      case _ :: tail => duplicateExposedValue(tail, seen)
      case Nil       => None

  private def unsupportedExposure(module: ast.Module): Option[(String, Span)] =
    module.exposing match
      case ast.ExposingExplicit(items) =>
        items.collectFirst {
          case operator @ ast.ExposedOperator(name)   => s"operator $name" -> operator.span
          case exposedType @ ast.ExposedType(name, _) => s"type $name"     -> exposedType.span
        }
      case exposeAll: ast.ExposingAll => Some("all values" -> exposeAll.span)

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

  private def lower(
      input: CompileInput,
      module: ast.Module,
      declaration: ast.ValueDeclaration
  ): MorphirIRFile =
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

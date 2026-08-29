package morphir.langkit.elm.compiler.mep

import kyo.*
import morphir.langkit.core.{SourceOffsets, Span}
import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast.{ExposedValue, ExposingExplicit}
import morphir.langkit.elm.compiler.ir.{CompileDiagnostic, CompileFailure, CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.codemodel as cm
import org.finos.morphir.codemodel.compat.v3.{V3ProjectionError, V3WireProjection}
import org.finos.morphir.naming.{ModuleName, Name, PackageName}

final case class ValidatedCompiledModel(
    distribution: cm.Distribution,
    packageName: PackageName,
    modules: Chunk[ModuleName]
) derives CanEqual

enum MepCompileError(val message: String) derives CanEqual:
  case InvalidParams(details: String)          extends MepCompileError(details)
  case InvalidCompilerOutput(details: String)  extends MepCompileError(details)
  case IRSerializationFailure(details: String) extends MepCompileError(details)

object MepCompileError:
  def jsonRpcCode(error: MepCompileError): Int = error match
    case _: MepCompileError.InvalidParams                                                     => -32602
    case _: MepCompileError.InvalidCompilerOutput | _: MepCompileError.IRSerializationFailure => -32603

object MepElmFrontend:
  private type Value = Structure.Value

  private val PackageIdentity =
    raw"(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*(?:/(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*)*".r
  private val ModuleIdentity = raw"[A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*".r

  def compile(params: Value): Result[MepCompileError, Value] =
    parseRequest(params) match
      case Right(request) => compileRequest(request)
      case Left(error)    => Result.fail(MepCompileError.InvalidParams(error))

  private[mep] def validateCompiledModel(
      distribution: cm.Distribution,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName]
  ): Either[String, ValidatedCompiledModel] =
    distribution match
      case cm.Distribution.Library(library) if library.packageInfo.name != requestedPackage =>
        Left("The compiled model package does not match the requested package")
      case cm.Distribution.Library(library) =>
        val modules = Chunk.from(library.definition.modules.keys.toVector.sortBy(_.toString))
        if modules.toSet != requestedModules then Left("The compiled model modules do not match the requested modules")
        else Right(ValidatedCompiledModel(distribution, library.packageInfo.name, modules))
      case _ => Left("The compiler returned a non-library distribution")

  private[mep] def validateCompilerOutput(
      distribution: cm.Distribution,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName]
  ): Either[MepCompileError, ValidatedCompiledModel] =
    validateCompiledModel(distribution, requestedPackage, requestedModules)
      .left
      .map(MepCompileError.InvalidCompilerOutput.apply)

  private[mep] def encodeCompilerOutput(
      distribution: cm.Distribution,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName],
      moduleSpellings: Chunk[String] = Chunk.empty
  ): Either[MepCompileError, Value] =
    for
      validated <- validateCompilerOutput(distribution, requestedPackage, requestedModules)
      ir        <- projectV3(validated.distribution)
    yield record(
      "success"     -> bool(true),
      "irVersion"   -> str("3"),
      "ir"          -> ir,
      "diagnostics" -> sequence(),
      "modules"     -> sequence(
        (if moduleSpellings.nonEmpty then moduleSpellings else validated.modules.map(_.toString)).map(str)*
      )
    )

  private def projectV3(distribution: cm.Distribution): Either[MepCompileError, Value] =
    V3WireProjection.project(distribution) match
      case Result.Success(value) => Right(value)
      case Result.Failure(error) => Left(MepCompileError.IRSerializationFailure(projectionMessage(error)))
      case Result.Panic(cause)   => Left(MepCompileError.IRSerializationFailure(cause.getMessage))

  private def projectionMessage(error: V3ProjectionError): String = error match
    case V3ProjectionError.UnsupportedDistribution(kind)     => s"Unsupported v3 distribution: $kind"
    case V3ProjectionError.UnsupportedFeature(path, feature) => s"Unsupported v3 feature at $path: $feature"
    case V3ProjectionError.InvalidModel(path, details)       => s"Invalid model at $path: $details"

  private def compileRequest(request: CompileRequest): Result[MepCompileError, Value] =
    val document = request.documents.head
    val module   = request.compilePackage.exposedModules.head
    val input    = CompileInput(
      source = document.text,
      packageName = PackageName.fromString(request.compilePackage.name),
      moduleName = ModuleName.fromString(module),
      exposedValues = sourceMetadata(document.text).map(_._2).getOrElse(Set.empty)
    )
    foldCompilerResult(
      document,
      input.packageName,
      input.moduleName,
      request.compilePackage.exposedModules,
      ElmToMorphirIRCompiler.compile(input)
    )

  private[mep] def foldCompilerResult(
      document: SourceDocument,
      packageName: PackageName,
      moduleName: ModuleName,
      moduleSpellings: Chunk[String],
      compilerResult: Result[CompileFailure, cm.Distribution]
  ): Result[MepCompileError, Value] =
    compilerResult match
      case Result.Success(distribution) =>
        encodeCompilerOutput(distribution, packageName, Set(moduleName), moduleSpellings) match
          case Right(value) => Result.succeed(value)
          case Left(error)  => Result.fail(error)
      case Result.Failure(failure) => Result.succeed(compileFailure(document, failure))
      case Result.Panic(cause)     => Result.panic(cause)

  private def compileFailure(document: SourceDocument, failure: CompileFailure): Value =
    record(
      "success"     -> bool(false),
      "diagnostics" -> sequence(failure.diagnostics.map(diagnosticValue(document, _))*),
      "modules"     -> sequence()
    )

  private def diagnosticValue(document: SourceDocument, diagnostic: CompileDiagnostic): Value =
    val span = diagnosticSpan(diagnostic)
    record(
      "severity" -> str("error"),
      "code"     -> str(diagnostic match
        case _: CompileDiagnostic.ParserFailure | _: CompileDiagnostic.MalformedModuleHeader => "elm.parser"
        case other                                                                           => other.code),
      "message"  -> str(diagnosticMessage(diagnostic)),
      "location" -> record("uri" -> str(document.uri), "range" -> sourceRange(document.text, span))
    )

  private[mep] def diagnosticMessage(diagnostic: CompileDiagnostic): String = diagnostic match
    case CompileDiagnostic.ParserFailure(parseDiagnostic)   => parseDiagnostic.message
    case _: CompileDiagnostic.MalformedModuleHeader         => "Malformed Elm module header"
    case CompileDiagnostic.UnsupportedModule(moduleType, _) =>
      val kind = moduleType match
        case morphir.langkit.elm.ast.ModuleType.Plain  => "plain"
        case morphir.langkit.elm.ast.ModuleType.Port   => "port"
        case morphir.langkit.elm.ast.ModuleType.Effect => "effect"
      s"Unsupported Elm module type: $kind"
    case CompileDiagnostic.UnsupportedImport(moduleName, _)        => s"Elm imports are not supported: $moduleName"
    case CompileDiagnostic.UnsupportedDeclaration(kind, _)         => s"Unsupported Elm declaration: $kind"
    case CompileDiagnostic.UnsupportedType(kind, _)                => s"Unsupported Elm type: $kind"
    case CompileDiagnostic.UnsupportedExpression(kind, _)          => s"Unsupported Elm expression: $kind"
    case CompileDiagnostic.UnsupportedPattern(kind, _)             => s"Unsupported Elm pattern: $kind"
    case CompileDiagnostic.ModuleNameMismatch(expected, actual, _) =>
      s"Expected Elm module $expected, but found $actual"
    case CompileDiagnostic.ExposedNameMismatch(expected, actual, _) =>
      s"Expected exposed values ${displayNames(expected)}, but found ${displayNames(actual)}"
    case CompileDiagnostic.UnsupportedExposure(kind, _) => s"Unsupported Elm exposure: $kind"
    case CompileDiagnostic.AnnotationNameMismatch(annotationName, declarationName, _) =>
      s"Type annotation $annotationName does not match declaration $declarationName"
    case CompileDiagnostic.DuplicateParameter(name, _)    => s"Duplicate Elm parameter: $name"
    case CompileDiagnostic.DuplicateExposedValue(name, _) => s"Duplicate exposed Elm value: $name"

  private def displayNames(names: Set[Name]): String = names.toVector.map(_.toString).sorted.mkString("[", ", ", "]")

  private def diagnosticSpan(diagnostic: CompileDiagnostic): Span = diagnostic match
    case CompileDiagnostic.ParserFailure(parseDiagnostic)     => parseDiagnostic.toSpan
    case CompileDiagnostic.MalformedModuleHeader(span)        => span
    case CompileDiagnostic.UnsupportedModule(_, span)         => span
    case CompileDiagnostic.UnsupportedImport(_, span)         => span
    case CompileDiagnostic.UnsupportedDeclaration(_, span)    => span
    case CompileDiagnostic.UnsupportedType(_, span)           => span
    case CompileDiagnostic.UnsupportedExpression(_, span)     => span
    case CompileDiagnostic.UnsupportedPattern(_, span)        => span
    case CompileDiagnostic.ModuleNameMismatch(_, _, span)     => span
    case CompileDiagnostic.ExposedNameMismatch(_, _, span)    => span
    case CompileDiagnostic.UnsupportedExposure(_, span)       => span
    case CompileDiagnostic.AnnotationNameMismatch(_, _, span) => span
    case CompileDiagnostic.DuplicateParameter(_, span)        => span
    case CompileDiagnostic.DuplicateExposedValue(_, span)     => span

  private def sourceRange(source: String, span: Span): Value =
    def position(offset: Int): Value =
      val (line, column) = SourceOffsets.lineColumnAt(source, offset)
      record("line" -> integer(line - 1), "character" -> integer(column - 1))
    record("start" -> position(span.start), "end" -> position(span.end))

  private def sourceMetadata(source: String): Option[(String, Set[Name])] =
    Elm.parseAst(source).toOption.map { module =>
      val exposedValues = module.exposing match
        case ExposingExplicit(items) => items.collect { case ExposedValue(name) => Name.fromString(name) }.toSet
        case _                       => Set.empty[Name]
      module.name.fullName -> exposedValues
    }

  private def parseRequest(value: Value): Either[String, CompileRequest] =
    Structure.decode[CompileRequest](value) match
      case Result.Success(request) => validateRequest(request)
      case Result.Failure(error)   => Left(s"Invalid compile parameters: ${error.getMessage}")
      case Result.Panic(error)     => Left(s"Invalid compile parameters: ${error.getMessage}")

  private def validateRequest(request: CompileRequest): Either[String, CompileRequest] =
    val documents = request.documents
    val modules   = request.compilePackage.exposedModules
    for
      _ <- Either.cond(request.languageId == "elm", (), "Morphir Scala Elm only compiles elm")
      _ <- Either.cond(documents.size == 1, (), "Exactly one source document is required")
      document = documents.head
      _ <- Either.cond(document.languageId == "elm", (), "The source document language must be elm")
      _ <- Either.cond(document.uri.trim.nonEmpty, (), "The source document URI must not be empty")
      _ <- Either.cond(request.dependencies.isEmpty, (), "dependencies are not supported")
      _ <- Either.cond(request.options.irVersion == "3", (), "Morphir Scala Elm only emits Morphir IR version 3")
      _ <- Either.cond(!request.options.typesOnly, (), "Types-only compilation is not supported")
      _ <- Either.cond(PackageIdentity.matches(request.compilePackage.name), (), "Invalid package identity")
      _ <- Either.cond(
        modules.size == 1 && ModuleIdentity.matches(modules.head),
        (),
        "Invalid exposed module identity"
      )
      _ <- sourceMetadata(document.text).map(_._1) match
        case Some(sourceModule) =>
          Either.cond(sourceModule == modules.head, (), "The exposed module does not match the source header")
        case None => Right(())
    yield request

  private def str(value: String): Value               = Structure.Value.Str(value)
  private def bool(value: Boolean): Value             = Structure.Value.Bool(value)
  private def integer(value: Int): Value              = Structure.Value.Integer(value)
  private def sequence(values: Value*): Value         = Structure.Value.Sequence(Chunk.from(values))
  private def record(fields: (String, Value)*): Value = Structure.Value.Record(Chunk.from(fields))

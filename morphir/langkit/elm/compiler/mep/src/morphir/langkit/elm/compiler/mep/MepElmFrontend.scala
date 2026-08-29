package morphir.langkit.elm.compiler.mep

import kyo.*
import morphir.langkit.core.{SourceOffsets, Span}
import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast.{ExposedValue, ExposingExplicit}
import morphir.langkit.elm.compiler.ir.{CompileDiagnostic, CompileFailure, CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.naming.{ModuleName, Name, PackageName}
import scala.util.Try
import zio.json.*
import zio.json.ast.Json

final case class ValidatedCompiledIR(
    ir: MorphirIRFile,
    packageName: PackageName,
    modules: Vector[ModuleName]
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
  private val MaxDocumentVersion = (BigInt(1) << 64) - 1
  private val PackageIdentity    =
    raw"(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*(?:/(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*)*".r
  private val ModuleIdentity = raw"[A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*".r

  def compile(params: Json): Result[MepCompileError, Json] =
    val result = parseRequest(params)
      .left.map(MepCompileError.InvalidParams.apply)
      .flatMap(compileRequest)
    result match
      case Right(value) => Result.succeed(value)
      case Left(error)  => Result.fail(error)

  private[mep] def validateCompiledIR(
      ir: MorphirIRFile,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName]
  ): Either[String, ValidatedCompiledIR] =
    if ir.version != MorphirIRVersion.V3_0 then Left("The compiler returned Morphir IR other than version 3")
    else
      ir.distribution match
        case library: Distribution.Library if library.packageName != requestedPackage =>
          Left("The compiled IR package does not match the requested package")
        case library: Distribution.Library =>
          val modules = library.packageDef.modules.keys.toVector.sortBy(_.toString)
          if modules.toSet != requestedModules then
            Left("The compiled IR modules do not match the requested modules")
          else Right(ValidatedCompiledIR(ir, library.packageName, modules))
        case _ => Left("The compiler returned a non-library distribution")

  private[mep] def validateCompilerOutput(
      ir: MorphirIRFile,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName]
  ): Either[MepCompileError, ValidatedCompiledIR] =
    validateCompiledIR(ir, requestedPackage, requestedModules).left.map(MepCompileError.InvalidCompilerOutput.apply)

  private[mep] def encodeCompilerOutput(
      ir: MorphirIRFile,
      requestedPackage: PackageName,
      requestedModules: Set[ModuleName],
      moduleSpellings: Vector[String] = Vector.empty
  ): Either[MepCompileError, Json] =
    for
      validated <- validateCompilerOutput(ir, requestedPackage, requestedModules)
      irJson    <- validated.ir.toJsonAST.left.map(MepCompileError.IRSerializationFailure.apply)
    yield Json.Obj(
      "success"     -> Json.Bool(true),
      "irVersion"   -> Json.Str("3"),
      "ir"          -> irJson,
      "diagnostics" -> Json.Arr(),
      "modules"     -> Json.Arr(
        Option.when(moduleSpellings.nonEmpty)(moduleSpellings)
          .getOrElse(validated.modules.map(_.toString))
          .map(Json.Str.apply)*
      )
    )

  private def compileRequest(request: CompileRequest): Either[MepCompileError, Json] =
    for
      document <- request.documents.headOption.toRight(
        MepCompileError.InvalidParams("Morphir Scala Elm requires exactly one source document")
      )
      module <- request.compilePackage.exposedModules.headOption.toRight(
        MepCompileError.InvalidParams("Exactly one exposed module is required")
      )
      exposedValues = sourceMetadata(document.text).map(_._2).getOrElse(Set.empty)
      input         = CompileInput(
        source = document.text,
        packageName = PackageName.fromString(request.compilePackage.name),
        moduleName = ModuleName.fromString(module),
        exposedValues = exposedValues,
        irVersion = MorphirIRVersion.V3_0
      )
      result <- ElmToMorphirIRCompiler.compile(input) match
        case Result.Success(ir) =>
          encodeCompilerOutput(ir, input.packageName, Set(input.moduleName), request.compilePackage.exposedModules)
        case Result.Failure(failure) => Right(compileFailure(document, failure))
    yield result

  private def compileFailure(document: SourceDocument, failure: CompileFailure): Json =
    Json.Obj(
      "success"     -> Json.Bool(false),
      "diagnostics" -> Json.Arr(failure.diagnostics.map(diagnosticJson(document, _))*),
      "modules"     -> Json.Arr()
    )

  private def diagnosticJson(document: SourceDocument, diagnostic: CompileDiagnostic): Json =
    val span = diagnosticSpan(diagnostic)
    Json.Obj(
      "severity" -> Json.Str("error"),
      "code"     -> Json.Str(diagnostic match
        case _: CompileDiagnostic.ParserFailure | _: CompileDiagnostic.MalformedModuleHeader => "elm.parser"
        case other                                                                           => other.code),
      "message"  -> Json.Str(diagnosticMessage(diagnostic)),
      "location" -> Json.Obj(
        "uri"   -> Json.Str(document.uri),
        "range" -> sourceRange(document.text, span)
      )
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
    case _: CompileDiagnostic.UnsupportedIRVersion      => "Unsupported Morphir IR version"
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
    case CompileDiagnostic.UnsupportedIRVersion(_)            => Span.zero
    case CompileDiagnostic.UnsupportedExposure(_, span)       => span
    case CompileDiagnostic.AnnotationNameMismatch(_, _, span) => span
    case CompileDiagnostic.DuplicateParameter(_, span)        => span
    case CompileDiagnostic.DuplicateExposedValue(_, span)     => span

  private def sourceRange(source: String, span: Span): Json =
    def position(offset: Int): Json =
      val (line, column) = SourceOffsets.lineColumnAt(source, offset)
      Json.Obj("line" -> Json.Num(line - 1), "character" -> Json.Num(column - 1))
    Json.Obj("start" -> position(span.start), "end" -> position(span.end))

  private def sourceMetadata(source: String): Option[(String, Set[Name])] =
    Elm.parseAst(source).toOption.map { module =>
      val exposedValues = module.exposing match
        case ExposingExplicit(items) => items.collect { case ExposedValue(name) => Name.fromString(name) }.toSet
        case _                       => Set.empty[Name]
      module.name.fullName -> exposedValues
    }

  private def parseRequest(value: Json): Either[String, CompileRequest] = value match
    case Json.Obj(fields) =>
      val values = fields.toMap
      for
        language  <- string(values, "languageId")
        documents <- values.get("documents") match
          case Some(Json.Arr(items)) =>
            items.toVector.foldRight(Right(Vector.empty): Either[String, Vector[SourceDocument]]) {
              (item, result) => for document <- parseDocument(item); tail <- result yield document +: tail
            }
          case _ => Left("documents must be an array")
        compilePackage <- values.get("package").toRight("package is required").flatMap(parsePackage)
        dependencies   <- values.get("dependencies") match
          case Some(Json.Arr(items)) if items.isEmpty => Right(Vector.empty)
          case Some(Json.Arr(_))                      => Left("dependencies are not supported")
          case _                                      => Left("dependencies must be an array")
        options <- values.get("options").toRight("options are required").flatMap(parseOptions)
        _       <- Either.cond(language == "elm", (), "Morphir Scala Elm only compiles elm")
        _       <- Either.cond(documents.size == 1, (), "Exactly one source document is required")
        _       <- Either.cond(documents.head.languageId == "elm", (), "The source document language must be elm")
        _       <- Either.cond(documents.head.uri.trim.nonEmpty, (), "The source document URI must not be empty")
        _       <- Either.cond(options.irVersion == "3", (), "Morphir Scala Elm only emits Morphir IR version 3")
        _       <- Either.cond(!options.typesOnly, (), "Types-only compilation is not supported")
        _       <- Either.cond(PackageIdentity.matches(compilePackage.name), (), "Invalid package identity")
        _       <- Either.cond(
          compilePackage.exposedModules.size == 1 && ModuleIdentity.matches(compilePackage.exposedModules.head),
          (),
          "Invalid exposed module identity"
        )
        _ <- sourceMetadata(documents.head.text).map(_._1) match
          case Some(sourceModule) =>
            Either.cond(
              sourceModule == compilePackage.exposedModules.head,
              (),
              "The exposed module does not match the source header"
            )
          case None => Right(())
      yield CompileRequest(language, documents, compilePackage, dependencies, options)
    case _ => Left("compile params must be an object")

  private def parseDocument(value: Json): Either[String, SourceDocument] = value match
    case Json.Obj(fields) =>
      val values = fields.toMap
      for
        uri      <- string(values, "uri")
        language <- string(values, "languageId")
        version  <- unsignedInteger(values, "version")
        text     <- string(values, "text")
      yield SourceDocument(uri, language, version, text)
    case _ => Left("document must be an object")

  private def parsePackage(value: Json): Either[String, CompilePackage] = value match
    case Json.Obj(fields) =>
      val values = fields.toMap
      for
        name    <- string(values, "name")
        modules <- values.get("exposedModules") match
          case Some(Json.Arr(items)) if items.forall(_.isInstanceOf[Json.Str]) =>
            Right(items.collect { case Json.Str(module) => module }.toVector)
          case _ => Left("exposedModules must contain strings")
      yield CompilePackage(name, modules)
    case _ => Left("package must be an object")

  private def parseOptions(value: Json): Either[String, CompileOptions] = value match
    case Json.Obj(fields) =>
      val values = fields.toMap
      for
        typesOnly <- values.get("typesOnly") match
          case Some(Json.Bool(value)) => Right(value)
          case _                      => Left("typesOnly must be a boolean")
        irVersion <- string(values, "irVersion")
      yield CompileOptions(typesOnly, irVersion)
    case _ => Left("options must be an object")

  private def string(values: Map[String, Json], field: String): Either[String, String] =
    values.get(field) match
      case Some(Json.Str(value)) => Right(value)
      case _                     => Left(s"$field must be a string")

  private def unsignedInteger(values: Map[String, Json], field: String): Either[String, BigInt] =
    values.get(field) match
      case Some(Json.Num(value)) =>
        Try(BigInt(value.toBigIntegerExact)).toOption
          .filter(number => number.signum >= 0 && number <= MaxDocumentVersion)
          .toRight(s"$field must be a non-negative integer")
      case _ => Left(s"$field must be a non-negative integer")

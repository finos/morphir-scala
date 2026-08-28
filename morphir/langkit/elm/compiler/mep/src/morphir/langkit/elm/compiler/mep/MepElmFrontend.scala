package morphir.langkit.elm.compiler.mep

import kyo.*
import morphir.langkit.core.{SourceOffsets, Span}
import morphir.langkit.elm.compiler.ir.{CompileDiagnostic, CompileFailure, CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.naming.{ModuleName, Name, PackageName}
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
  private val ModuleHeader = raw"(?m)^module\s+([A-Z][A-Za-z0-9]*(?:\.[A-Z][A-Za-z0-9]*)*)\s+exposing\s*\(([^)]*)\)".r
  private val PackageIdentity =
    raw"(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*(?:/(?:[a-z]+|[0-9]+)(?:-(?:[a-z]+|[0-9]+))*)*".r
  private val ModuleIdentity = raw"[A-Z][A-Za-z0-9]*(?:\.[A-Z][A-Za-z0-9]*)*".r

  def compile(params: Json): Either[MepCompileError, Json] =
    parseRequest(params)
      .left.map(MepCompileError.InvalidParams.apply)
      .flatMap(compileRequest)

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
      requestedModules: Set[ModuleName]
  ): Either[MepCompileError, Json] =
    for
      validated <- validateCompilerOutput(ir, requestedPackage, requestedModules)
      irJson    <- validated.ir.toJsonAST.left.map(MepCompileError.IRSerializationFailure.apply)
    yield Json.Obj(
      "success"     -> Json.Bool(true),
      "irVersion"   -> Json.Str("3"),
      "ir"          -> irJson,
      "diagnostics" -> Json.Arr(),
      "modules"     -> Json.Arr(validated.modules.map(name => Json.Str(name.toString))*)
    )

  private def compileRequest(request: CompileRequest): Either[MepCompileError, Json] =
    for
      document <- request.documents.headOption.toRight(
        MepCompileError.InvalidParams("Morphir Scala Elm requires exactly one source document")
      )
      module <- request.compilePackage.exposedModules.headOption.toRight(
        MepCompileError.InvalidParams("Exactly one exposed module is required")
      )
      exposedValues = sourceExposedValues(document.text)
      input         = CompileInput(
        source = document.text,
        packageName = PackageName.fromString(request.compilePackage.name),
        moduleName = ModuleName.fromString(module),
        exposedValues = exposedValues,
        irVersion = MorphirIRVersion.V3_0
      )
      result <- ElmToMorphirIRCompiler.compile(input) match
        case Result.Success(ir)      => encodeCompilerOutput(ir, input.packageName, Set(input.moduleName))
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
      "message" -> Json.Str(diagnostic match
        case CompileDiagnostic.ParserFailure(parseDiagnostic) => parseDiagnostic.message
        case _: CompileDiagnostic.MalformedModuleHeader       => "Malformed Elm module header"
        case other                                            => other.toString),
      "location" -> Json.Obj(
        "uri"   -> Json.Str(document.uri),
        "range" -> sourceRange(document.text, span)
      )
    )

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

  private def sourceExposedValues(source: String): Set[Name] =
    ModuleHeader.findFirstMatchIn(source).toSet.flatMap { matched =>
      matched.group(
        2
      ).split(',').iterator.map(_.trim).filter(value => value.nonEmpty && value != "..").map(Name.fromString)
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
        _       <- Either.cond(PackageIdentity.matches(compilePackage.name), (), "Invalid package identity")
        _       <- Either.cond(
          compilePackage.exposedModules.size == 1 && ModuleIdentity.matches(compilePackage.exposedModules.head),
          (),
          "Invalid exposed module identity"
        )
        _ <- ModuleHeader.findFirstMatchIn(documents.head.text).map(_.group(1)) match
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
        version  <- integer(values, "version")
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

  private def integer(values: Map[String, Json], field: String): Either[String, Int] =
    values.get(field) match
      case Some(Json.Num(value)) =>
        Option(value.toBigIntegerExact)
          .filter(number => number.signum >= 0 && number.bitLength <= 31)
          .map(_.intValue)
          .toRight(s"$field must be a non-negative integer")
      case _ => Left(s"$field must be a non-negative integer")

package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.schema.*
import morphir.langkit.elm.compiler.mep.internal.ExtensionDefinition

final case class LanguageMetadata(id: String, fileExtensions: Chunk[String]) derives CanEqual, Schema

final case class ProviderMetadata(
    id: String,
    name: String,
    version: String,
    protocolVersion: String,
    types: Chunk[String],
    languages: Chunk[LanguageMetadata],
    irVersions: Chunk[String],
    compile: Boolean
) derives CanEqual, Schema

object ProviderMetadata:
  private[mep] val defaultCapabilities: ExtensionCapabilities =
    Json.decode[ExtensionCapabilities](ExtensionDefinition.CapabilitiesJson).getOrThrow

  val default: ProviderMetadata = ProviderMetadata(
    id = ExtensionDefinition.Id,
    name = ExtensionDefinition.Name,
    version = ExtensionDefinition.DefaultVersion,
    protocolVersion = ExtensionDefinition.ProtocolVersion,
    types = Chunk.from(ExtensionDefinition.Types),
    languages = defaultCapabilities.frontend.languages,
    irVersions = defaultCapabilities.frontend.irVersions,
    compile = defaultCapabilities.frontend.compile
  )

opaque type DocumentVersion = BigInt

object DocumentVersion:
  val Min: DocumentVersion = BigInt(0)
  val Max: DocumentVersion = (BigInt(1) << 64) - 1

  def apply(value: Int): DocumentVersion    = apply(BigInt(value))
  def apply(value: Long): DocumentVersion   = apply(BigInt(value))
  def apply(value: BigInt): DocumentVersion =
    require(isValid(value), s"Document version must be between $Min and $Max")
    value

  extension (version: DocumentVersion) def toBigInt: BigInt = version

  given CanEqual[DocumentVersion, DocumentVersion] = CanEqual.derived

  given Schema[DocumentVersion] = Schema.init[DocumentVersion](
    writeFn = (version, writer) =>
      if version.isValidLong then writer.long(version.longValue)
      else writer.bigDecimal(BigDecimal(version)),
    readFn = reader => decode(reader.bigDecimal())(using reader.frame),
    structure = Structure.Type.Primitive(
      Structure.PrimitiveKind.BigInt,
      Tag[DocumentVersion].asInstanceOf[Tag[Any]]
    )
  )

  private def isValid(value: BigInt): Boolean = value >= Min && value <= Max

  private def decode(number: BigDecimal)(using Frame): DocumentVersion =
    number.toBigIntExact.filter(isValid) match
      case Some(version) => version
      case None          => throw TypeMismatchException(Seq.empty, "unsigned 64-bit integer", number.toString)

final case class SourceDocument(uri: String, languageId: String, version: DocumentVersion, text: String)
    derives CanEqual, Schema

final case class CompilePackage(name: String, exposedModules: Chunk[String]) derives CanEqual, Schema

final case class CompileOptions(typesOnly: Boolean, irVersion: String) derives CanEqual, Schema

final case class CompileRequest(
    languageId: String,
    documents: Chunk[SourceDocument],
    @rename("package") compilePackage: CompilePackage,
    dependencies: Chunk[JsonDependency],
    options: CompileOptions
) derives CanEqual, Schema

object CompileRequest:
  /** Decode either compile envelope into the document-based request used by the compiler. */
  def decode(value: Structure.Value): Result[String, CompileRequest] =
    normalize(value).flatMap { normalized =>
      Structure.decode[CompileRequest](normalized) match
        case Result.Success(request) => Result.succeed(request)
        case Result.Failure(error)   => Result.fail(s"Invalid compile parameters: ${error.getMessage}")
        case Result.Panic(error)     => Result.fail(s"Invalid compile parameters: ${error.getMessage}")
    }

  private def normalize(value: Structure.Value): Result[String, Structure.Value] = value match
    case Structure.Value.Record(fields) =>
      val members = fields.iterator.toMap
      (members.contains("sources"), members.contains("documents")) match
        case (true, true) =>
          Result.fail("Ambiguous morphir.frontend.compile parameters: provide either sources or documents, not both")
        case (false, false) =>
          Result.fail("Invalid morphir.frontend.compile parameters: missing sources or documents")
        case (false, true) => Result.succeed(value)
        case (true, false) => members("sources") match
            case Structure.Value.Record(sources) =>
              if sources.exists { case (name, root) => name == "root" && !root.isInstanceOf[Structure.Value.Str] } then
                Result.fail("Invalid morphir.frontend.compile parameters: sources.root must be a string")
              else
                Result.succeed(Structure.Value.Record(
                  fields.filter(_._1 != "sources") ++ sources.filter(_._1 == "documents")
                ))
            case _ => Result.fail("Invalid morphir.frontend.compile parameters: sources must be an object")
    case _ => Result.succeed(value)

final case class JsonDependency(packageName: String, irVersion: String, distribution: Structure.Value)
    derives CanEqual, Schema

final case class HostMetadata(name: String, version: String) derives CanEqual, Schema

final case class InitializeRequest(protocolVersions: Chunk[String], host: HostMetadata) derives CanEqual, Schema

final case class DescribeRequest(protocolVersions: Chunk[String]) derives CanEqual, Schema

final case class ExtensionClaims(
    claimsVersion: String,
    protocolVersions: Chunk[String],
    extension: ExtensionInfo,
    capabilities: ExtensionCapabilities
) derives CanEqual, Schema

final case class ExtensionInfo(id: String, name: String, version: String, types: Chunk[String]) derives CanEqual, Schema

final case class FrontendCapabilities(
    languages: Chunk[LanguageMetadata],
    irVersions: Chunk[String],
    compile: Boolean,
    incremental: Boolean,
    fragments: Boolean,
    multiDocument: Boolean
) derives CanEqual, Schema

final case class WorkspaceCapabilities(protocolVersions: Chunk[String], discover: Boolean) derives CanEqual, Schema

final case class ExtensionCapabilities(
    frontend: FrontendCapabilities,
    workspace: WorkspaceCapabilities,
    streaming: Boolean,
    incremental: Boolean,
    cancellation: Boolean,
    progress: Boolean
) derives CanEqual, Schema

final case class InitializationResult(
    protocolVersion: String,
    extension: ExtensionInfo,
    capabilities: ExtensionCapabilities
) derives CanEqual, Schema

final case class PingResult(ok: Boolean) derives CanEqual, Schema

enum SessionState derives CanEqual:
  case Loaded, Ready, AwaitExit, Stopped, Failed

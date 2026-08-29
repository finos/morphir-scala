package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.schema.*

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
  val default: ProviderMetadata = ProviderMetadata(
    id = "morphir-scala-elm",
    name = "Morphir Scala Elm frontend",
    version = "0.1.0",
    protocolVersion = "0.1",
    types = Chunk("frontend"),
    languages = Chunk(LanguageMetadata("elm", Chunk(".elm"))),
    irVersions = Chunk("3"),
    compile = true
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

final case class JsonDependency(packageName: String, irVersion: String, distribution: Structure.Value)
    derives CanEqual, Schema

final case class HostMetadata(name: String, version: String) derives CanEqual, Schema

final case class InitializeRequest(protocolVersions: Chunk[String], host: HostMetadata) derives CanEqual, Schema

final case class ExtensionInfo(id: String, name: String, version: String, types: Chunk[String]) derives CanEqual, Schema

final case class FrontendCapabilities(
    languages: Chunk[LanguageMetadata],
    irVersions: Chunk[String],
    compile: Boolean,
    incremental: Boolean,
    fragments: Boolean
) derives CanEqual, Schema

final case class ExtensionCapabilities(
    frontend: FrontendCapabilities,
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
  case Loaded, Ready, AwaitExit, Stopped

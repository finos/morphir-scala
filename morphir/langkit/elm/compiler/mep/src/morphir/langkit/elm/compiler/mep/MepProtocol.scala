package morphir.langkit.elm.compiler.mep

final case class LanguageMetadata(id: String, fileExtensions: Vector[String]) derives CanEqual

final case class ProviderMetadata(
    id: String,
    name: String,
    version: String,
    protocolVersion: String,
    types: Vector[String],
    languages: Vector[LanguageMetadata],
    irVersions: Vector[String],
    compile: Boolean
) derives CanEqual

object ProviderMetadata:
  val default: ProviderMetadata = ProviderMetadata(
    id = "morphir-scala-elm",
    name = "Morphir Scala Elm frontend",
    version = "0.1.0",
    protocolVersion = "0.1",
    types = Vector("frontend"),
    languages = Vector(LanguageMetadata("elm", Vector(".elm"))),
    irVersions = Vector("3"),
    compile = true
  )

final case class SourceDocument(uri: String, languageId: String, version: Int, text: String) derives CanEqual

final case class CompilePackage(name: String, exposedModules: Vector[String]) derives CanEqual

final case class CompileOptions(typesOnly: Boolean, irVersion: String) derives CanEqual

final case class CompileRequest(
    languageId: String,
    documents: Vector[SourceDocument],
    compilePackage: CompilePackage,
    dependencies: Vector[JsonDependency],
    options: CompileOptions
) derives CanEqual

final case class JsonDependency(packageName: String, irVersion: String, distribution: zio.json.ast.Json)
    derives CanEqual

enum SessionState derives CanEqual:
  case Loaded, Ready, Stopped

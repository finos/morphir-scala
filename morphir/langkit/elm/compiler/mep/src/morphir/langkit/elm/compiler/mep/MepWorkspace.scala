package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8

import kyo.*

/**
 * Ad-hoc workspace discovery for Elm sources (`morphir.workspace.discover`).
 *
 * A host compiles a selection of files as a project that the provider synthesizes. The wire types, the order of the
 * checks, the failure codes and the failure messages follow the portable discovery engine in finos/morphir-rust
 * (`morphir-workspace`, `discover_with_identity`) and the Elm extension in finos/morphir-elm (`cli2/mep/workspace.ts`),
 * so a host gets the same answer from every Elm provider. Only the ad-hoc-sources purpose is served; the host discovers
 * manifest projects itself.
 */
private[mep] object MepWorkspace:
  private type Value = Structure.Value

  /**
   * The workspace discovery protocol version this provider writes, a SemVer string. The protocol is a draft, so a
   * reader names the exact draft it speaks: a prerelease matches only exactly.
   */
  val ProtocolVersion: String = "0.1.0-draft.1"

  enum FileEntry derives CanEqual:
    case Directory
    case File(text: String)
    case Symlink(target: String)

  /** Entries in the byte order of their UTF-8 paths, the order the reference implementations iterate. */
  final case class FileTree(entries: Chunk[(String, FileEntry)]) derives CanEqual:
    def fileText(path: String): Option[String] = entries.collectFirst {
      case (`path`, FileEntry.File(text)) => text
    }

  enum ProjectSource derives CanEqual:
    case Synthesized
    case Manifest(path: String)

  final case class SourceSelection(root: String, paths: Chunk[String]) derives CanEqual

  enum DiscoveryPurpose derives CanEqual:
    case ManifestProjects
    case AdHocSources(project: ProjectSource, sources: SourceSelection, languageId: String)

  final case class DiscoveryRequest(
      protocolVersion: String,
      developmentRoot: FileTree,
      morphirHome: Option[FileTree],
      systemConfig: Option[FileTree],
      cliOverlay: Value,
      purpose: DiscoveryPurpose
  ) derives CanEqual

  private final case class Refusal(code: String, message: String, path: Option[String])

  private val SemVer =
    raw"(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?".r

  /** A word of an Elm name, as morphir-elm `Name.fromString` reads it. */
  private val NameWord = raw"[a-zA-Z][a-z]*|[0-9]+".r

  private val DriveLetter = raw"[A-Za-z]:.*".r

  /** Whether `version` is a version in canonical SemVer spelling. */
  def isSemVer(version: String): Boolean = SemVer.matches(version)

  /** Whether this provider speaks workspace discovery protocol `version`. Build metadata does not take part. */
  def speaks(version: String): Boolean =
    isSemVer(version) && version.takeWhile(_ != '+') == ProtocolVersion

  // ---- Request parsing: a malformed request is invalid params, not a discovery failure.

  /** Reads `morphir.workspace.discover` parameters, or explains why they are not a discovery request. */
  def parseRequest(params: Option[Value]): Either[String, DiscoveryRequest] =
    params match
      case Some(Structure.Value.Record(fields)) => request(fields.iterator.toMap)
      case _                                    => Left("morphir.workspace.discover parameters must be an object")

  private def request(value: Map[String, Value]): Either[String, DiscoveryRequest] =
    for
      protocolVersion <- value.get("protocolVersion") match
        case Some(Structure.Value.Str(version)) if isSemVer(version) => Right(version)
        case _                                                       => Left("protocolVersion must be a SemVer version")
      _ <- Either.cond(environmentIsValid(value.get("environment")), (), "environment must map names to strings")
      developmentRoot <- fileTree("developmentRoot", value.get("developmentRoot"))
      morphirHome     <- optionalFileTree("morphirHome", value.get("morphirHome"))
      systemConfig    <- optionalFileTree("systemConfig", value.get("systemConfig"))
      purpose         <- purpose(value.get("purpose"))
    yield DiscoveryRequest(
      protocolVersion,
      developmentRoot,
      morphirHome,
      systemConfig,
      value.getOrElse("cliOverlay", Structure.Value.Null),
      purpose
    )

  private def environmentIsValid(value: Option[Value]): Boolean = value match
    case None                                 => true
    case Some(Structure.Value.Record(fields)) => fields.forall(_._2.isInstanceOf[Structure.Value.Str])
    case _                                    => false

  /** A canonical path confined to its mount, as morphir-workspace `RelativePath` accepts it. */
  def isRelativePath(path: String): Boolean =
    path == "." || (
      path.nonEmpty && !path.startsWith("/") && !path.contains('\\') && !DriveLetter.matches(path) &&
        path.split("/", -1).forall(segment => segment.nonEmpty && segment != "." && segment != "..")
    )

  private def relativePath(value: Option[Value]): Option[String] = value.collect {
    case Structure.Value.Str(path) if isRelativePath(path) => path
  }

  private def fieldsOf(value: Option[Value]): Map[String, Value] = value match
    case Some(Structure.Value.Record(fields)) => fields.iterator.toMap
    case _                                    => Map.empty

  private def utf8Order(left: String, right: String): Boolean =
    java.util.Arrays.compareUnsigned(left.getBytes(UTF_8), right.getBytes(UTF_8)) < 0

  private def fileTree(name: String, value: Option[Value]): Either[String, FileTree] =
    val entries = value match
      case Some(Structure.Value.Record(_)) => fieldsOf(value).get("entries")
      case _                               => None
    entries match
      case Some(Structure.Value.Record(entries)) =>
        val sorted = entries.iterator.toMap.toVector.sortWith((left, right) => utf8Order(left._1, right._1))
        traverse(Chunk.from(sorted)) { (path, entry) =>
          if isRelativePath(path) then fileEntry(path, entry).map(path -> _)
          else Left(s"$name path $path is not confined to its mount")
        }.map(FileTree.apply)
      case _ => Left(s"$name must be a file tree")

  private def optionalFileTree(name: String, value: Option[Value]): Either[String, Option[FileTree]] = value match
    case None | Some(Structure.Value.Null) => Right(None)
    case tree                              => fileTree(name, tree).map(Some(_))

  private def fileEntry(path: String, value: Value): Either[String, FileEntry] =
    val fields = fieldsOf(Some(value))
    (fields.get("kind"), fields.get("text"), relativePath(fields.get("target"))) match
      case (Some(Structure.Value.Str("directory")), _, _)                          => Right(FileEntry.Directory)
      case (Some(Structure.Value.Str("file")), Some(Structure.Value.Str(text)), _) => Right(FileEntry.File(text))
      case (Some(Structure.Value.Str("symlink")), _, Some(target))                 => Right(FileEntry.Symlink(target))
      case _ => Left(s"Invalid file tree entry at $path")

  private def purpose(value: Option[Value]): Either[String, DiscoveryPurpose] =
    val fields = fieldsOf(value)
    (value, fields.get("kind")) match
      case (None, _)                                           => Right(DiscoveryPurpose.ManifestProjects)
      case (_, Some(Structure.Value.Str("manifest-projects"))) => Right(DiscoveryPurpose.ManifestProjects)
      case (Some(Structure.Value.Record(_)), Some(Structure.Value.Str("ad-hoc-sources"))) =>
        fields.get("languageId") match
          case Some(Structure.Value.Str(languageId)) =>
            for
              project <- projectSource(fields.get("project"))
              sources <- sources(fields.get("sources"))
            yield DiscoveryPurpose.AdHocSources(project, sources, languageId)
          case _ => Left("An ad-hoc discovery purpose requires a languageId")
      case _ => Left("Invalid workspace discovery purpose")

  private def projectSource(value: Option[Value]): Either[String, ProjectSource] =
    val fields = fieldsOf(value)
    (fields.get("kind"), relativePath(fields.get("path"))) match
      case (Some(Structure.Value.Str("synthesized")), _)       => Right(ProjectSource.Synthesized)
      case (Some(Structure.Value.Str("manifest")), Some(path)) => Right(ProjectSource.Manifest(path))
      case _                                                   => Left("Invalid ad-hoc project source")

  private def sources(value: Option[Value]): Either[String, SourceSelection] =
    val fields = fieldsOf(value)
    (relativePath(fields.get("root")), fields.get("paths")) match
      case (Some(root), Some(Structure.Value.Sequence(paths))) =>
        traverse(paths)(path => relativePath(Some(path)).toRight("Invalid ad-hoc source selection"))
          .map(SourceSelection(root, _))
      case _ => Left("Invalid ad-hoc source selection")

  private def traverse[A, B](values: Chunk[A])(f: A => Either[String, B]): Either[String, Chunk[B]] =
    values.foldLeft[Either[String, Chunk[B]]](Right(Chunk.empty)) { (done, value) =>
      done.flatMap(results => f(value).map(results :+ _))
    }

  // ---- Discovery: a request that parses is answered with a success or a failure response.

  /** Answers a discovery request with a `DiscoveryResponse` wire value. */
  def discover(request: DiscoveryRequest): Value =
    snapshot(request) match
      case Right(snapshot) => record("status" -> str("success"), "snapshot" -> snapshot)
      case Left(refusal)   =>
        record(
          "status" -> str("failure"),
          "error"  -> record(
            "code"    -> str(refusal.code),
            "message" -> str(refusal.message),
            "path"    -> optional(refusal.path)
          )
        )

  private def refuse(code: String, message: String, path: Option[String]): Left[Refusal, Nothing] =
    Left(Refusal(code, message, path))

  private def check(condition: Boolean)(refusal: => Left[Refusal, Nothing]): Either[Refusal, Unit] =
    if condition then Right(()) else refusal

  private def snapshot(request: DiscoveryRequest): Either[Refusal, Value] =
    for
      _ <- check(speaks(request.protocolVersion))(
        refuse(
          "workspace.protocol.unsupported",
          s"unsupported workspace discovery protocol ${request.protocolVersion}; supported version is $ProtocolVersion",
          None
        )
      )
      _ <- check(request.cliOverlay == Structure.Value.Null || request.cliOverlay.isInstanceOf[Structure.Value.Record])(
        refuse("workspace.config.invalid", "CLI overlay must be a JSON object or null", None)
      )
      _        <- rejectSymlinks(Some(request.developmentRoot), "development root")
      _        <- rejectSymlinks(request.morphirHome, "Morphir Home")
      _        <- rejectSymlinks(request.systemConfig, "system configuration")
      snapshot <- request.purpose match
        case DiscoveryPurpose.ManifestProjects =>
          refuse(
            "workspace.purpose.unsupported",
            "the Morphir Scala Elm extension discovers ad-hoc sources only; manifest projects are discovered by the host",
            None
          )
        case DiscoveryPurpose.AdHocSources(project, sources, languageId) =>
          adHocSources(request, project, sources, languageId)
    yield snapshot

  private def rejectSymlinks(tree: Option[FileTree], mount: String): Either[Refusal, Unit] =
    tree.flatMap(_.entries.collectFirst { case (path, FileEntry.Symlink(target)) => path -> target }) match
      case Some((path, target)) =>
        refuse(
          "workspace.symlink.unsupported",
          s"unmaterialized symlink `$path` to `$target` in $mount; protocol-v1 hosts must materialize confined " +
            "symlink targets before discovery",
          Some(path)
        )
      case None => Right(())

  private def adHocSources(
      request: DiscoveryRequest,
      project: ProjectSource,
      sources: SourceSelection,
      languageId: String
  ): Either[Refusal, Value] =
    val root = sources.root
    for
      _ <- check(languageId.nonEmpty)(
        refuse(
          "workspace.language-id.empty",
          s"ad-hoc selection rooted at `$root` has an empty language id",
          Some(root)
        )
      )
      explicitName <- explicitProjectName(request.cliOverlay, root)
      _            <- project match
        case ProjectSource.Manifest(path) =>
          for
            _ <- check(explicitName.isDefined)(
              refuse(
                "workspace.selection.name-required",
                s"ad-hoc selection borrowing manifest `$path` has no explicit name; the host states the manifest's " +
                  "project name as the overlay's `project.name`",
                Some(path)
              )
            )
            _ <- check(request.developmentRoot.fileText(path).isDefined)(
              refuse("workspace.selection.invalid", s"manifest `$path` is not a file in the request", Some(path))
            )
          yield ()
        case ProjectSource.Synthesized => Right(())
      _          <- validateSelection(request.developmentRoot, sources)
      normalName <- explicitName match
        case Some(name) => normalProjectName(name, root).map(Some(_))
        case None       =>
          check(sources.paths.size == 1)(
            refuse(
              "workspace.selection.name-required",
              s"ad-hoc selection rooted at `$root` selects ${sources.paths.size} sources but has no explicit name; " +
                "an unnamed synthesized selection must select exactly one source",
              Some(root)
            )
          ).map(_ => None)
      modules <- selectedModules(request.developmentRoot, sources.paths)
    yield record(
      "protocolVersion" -> str(ProtocolVersion),
      "configAnchor"    -> Structure.Value.Null,
      "name"            -> Structure.Value.Null,
      "state"           -> str("open"),
      "projects"        -> sequence(
        record(
          "name"         -> str(normalName.getOrElse(synthesizedPackageName(modules.head))),
          "version"      -> Structure.Value.Null,
          "relativePath" -> str(root),
          "configAnchor" -> optional(project match
            case ProjectSource.Manifest(path) => Some(path)
            case ProjectSource.Synthesized    => None),
          "sourceDirectory" -> str("."),
          "state"           -> str("unloaded"),
          "diagnostics"     -> sequence(),
          "origin"          ->
            (project match
              case ProjectSource.Manifest(path) => record("kind" -> str("manifest"), "path" -> str(path))
              case ProjectSource.Synthesized    =>
                record("kind" -> str("synthesized"), "inputs" -> sequence(sources.paths.map(str)*))),
          "exposedModules" -> sequence(modules.map(str)*)
        )
      ),
      "diagnostics" -> sequence()
    )

  /**
   * An explicit name comes only from the overlay's `project.name`. It must be a string that is not blank, and it is
   * kept trimmed of Unicode whitespace.
   */
  private def explicitProjectName(cliOverlay: Value, root: String): Either[Refusal, Option[String]] =
    val project = cliOverlay match
      case Structure.Value.Record(fields) => fields.iterator.toMap.get("project")
      case _                              => None
    project match
      case Some(Structure.Value.Record(fields)) => fields.iterator.toMap.get("name") match
          case None                                                        => Right(None)
          case Some(Structure.Value.Str(name)) if trimSpace(name).nonEmpty => Right(Some(trimSpace(name)))
          case Some(Structure.Value.Str(_))                                =>
            refuse(
              "workspace.project-name.empty",
              "CLI overlay `project.name` must not be empty or whitespace-only",
              Some(root)
            )
          case Some(other) =>
            refuse(
              "workspace.config.invalid",
              s"CLI overlay `project.name` must be a string, found `${Json.encode(other)}`",
              Some(root)
            )
      case _ => Right(None)

  /**
   * The normal form of an explicit name. The name splits into package path segments on both `/` and `.`, each segment
   * splits into words as morphir-elm `Name.fromString` does, and the normal form joins the words with `-` and the
   * segments with `/`. `My.Package`, `My/Package` and `my/package` thus name the same package, and the normal form
   * always satisfies the compile request's package identity.
   */
  private def normalProjectName(name: String, root: String): Either[Refusal, String] =
    val pieces = name.split("[/.]").iterator.map(trimSpace).filter(_.nonEmpty).toSeq
    val words  = pieces.map(piece => piece -> NameWord.findAllIn(piece).map(_.toLowerCase).toSeq)
    for
      _ <- check(pieces.nonEmpty)(
        refuse(
          "workspace.project-name.invalid",
          s"project name `$name` is invalid: it names no package path segments",
          Some(root)
        )
      )
      _ <- words.collectFirst { case (piece, Seq()) => piece } match
        case Some(piece) =>
          refuse(
            "workspace.project-name.invalid",
            s"project name `$name` is invalid: segment `$piece` has no letters or digits",
            Some(root)
          )
        case None => Right(())
    yield words.map(_._2.mkString("-")).mkString("/")

  /**
   * The text without leading and trailing whitespace, as Rust `str::trim` and JavaScript `String.prototype.trim` read
   * it: the Unicode `White_Space` characters, and U+FEFF. `String.trim` would also strip the other control characters
   * below U+0020, and `String.strip` would keep U+00A0.
   */
  private def trimSpace(text: String): String =
    def isSpace(character: Char): Boolean = (character >= '\t' && character <= '\r') || character == '\u0085' ||
      character == '\uFEFF' ||
      Character.isSpaceChar(character)
    text.dropWhile(isSpace).reverse.dropWhile(isSpace).reverse

  private def validateSelection(tree: FileTree, sources: SourceSelection): Either[Refusal, Unit] =
    val repeated    = sources.paths.diff(sources.paths.distinct).headOption
    val outsideRoot = sources.paths.filterNot(isUnderRoot(sources.root, _))
    val notFiles    = sources.paths.filter(tree.fileText(_).isEmpty)
    for
      _ <- check(sources.paths.nonEmpty)(
        refuse(
          "workspace.selection.empty",
          s"ad-hoc selection rooted at `${sources.root}` selects no sources",
          Some(sources.root)
        )
      )
      _ <- check(repeated.isEmpty)(
        refuse(
          "workspace.selection.duplicate",
          s"selected path `${repeated.get}` is repeated in the selection",
          repeated
        )
      )
      _ <- check(outsideRoot.isEmpty)(
        refuse(
          "workspace.selection.outside-root",
          s"selected paths are not under selection root `${sources.root}`: ${listed(outsideRoot)}",
          outsideRoot.headOption
        )
      )
      _ <- check(notFiles.isEmpty)(
        refuse(
          "workspace.selection.invalid",
          s"selected paths do not resolve to files: ${listed(notFiles)}",
          notFiles.headOption
        )
      )
    yield ()

  /** Each selected source's module, in selection order; two sources may not name one module. */
  private def selectedModules(tree: FileTree, paths: Chunk[String]): Either[Refusal, Chunk[String]] =
    paths.foldLeft[Either[Refusal, (Chunk[String], Map[String, String])]](Right(Chunk.empty -> Map.empty)) {
      case (Right((modules, definedBy)), path) =>
        val module = ElmSourceNames.moduleName(fileName(path), tree.fileText(path).getOrElse(""))
        definedBy.get(module) match
          case Some(first) =>
            refuse(
              "workspace.selection.module-collision",
              s"selected sources `$first` and `$path` both define module `$module`",
              Some(path)
            )
          case None => Right((modules :+ module) -> definedBy.updated(module, path))
      case (refused, _) => refused
    }.map(_._1)

  /** The historical single-file rule: ASCII-lowercase the module and join its segments with `-`. */
  private def synthesizedPackageName(module: String): String =
    "local/" + module.map(character => if character >= 'A' && character <= 'Z' then character.toLower else character)
      .replace('.', '-')

  /** Whether `path` lies strictly beneath `root`, comparing path segments rather than string prefixes. */
  private def isUnderRoot(root: String, path: String): Boolean =
    val rootSegments = segments(root)
    val pathSegments = segments(path)
    pathSegments.length > rootSegments.length && pathSegments.startsWith(rootSegments)

  private def segments(path: String): Vector[String] =
    if path == "." then Vector.empty else path.split("/", -1).toVector

  private def fileName(path: String): String = path.substring(path.lastIndexOf('/') + 1)

  private def listed(paths: Chunk[String]): String = paths.map(path => s"`$path`").mkString(", ")

  private def optional(value: Option[String]): Value = value.map(str).getOrElse(Structure.Value.Null)

  private def str(value: String): Value               = Structure.Value.Str(value)
  private def sequence(values: Value*): Value         = Structure.Value.Sequence(Chunk.from(values))
  private def record(fields: (String, Value)*): Value = Structure.Value.Record(Chunk.from(fields))
end MepWorkspace

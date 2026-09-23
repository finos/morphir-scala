package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*

// The cases follow the reference implementations of ad-hoc discovery: morphir-workspace and the Elm policy in
// morphir-elm-binding (finos/morphir-rust), and cli2/mep/workspace.test.ts (finos/morphir-elm).
class MepWorkspaceTests extends Test[Any]:
  private type Value = Structure.Value

  private val widget = "module Acme.Widget exposing (Size)\n\n\ntype alias Size =\n    Int\n"
  private val gadget = "module Acme.Gadget exposing (Size)\n"

  private def str(value: String): Value            = Structure.Value.Str(value)
  private def arr(values: Value*): Value           = Structure.Value.Sequence(Chunk.from(values))
  private def obj(fields: (String, Value)*): Value = Structure.Value.Record(Chunk.from(fields))
  private def strs(values: String*): Value         = arr(values.map(str)*)

  private def json(text: String): Value = Json.decode[Value](text) match
    case Result.Success(value) => value
    case other                 => throw AssertionError(s"test JSON did not decode: $other")

  private val directory: Value               = obj("kind" -> str("directory"))
  private def file(text: String): Value      = obj("kind" -> str("file"), "text" -> str(text))
  private def symlink(target: String): Value = obj("kind" -> str("symlink"), "target" -> str(target))

  private def files(texts: (String, String)*): Chunk[(String, Value)] =
    Chunk("." -> directory) ++ Chunk.from(texts.map((path, text) => path -> file(text)))

  private val synthesized: Value            = obj("kind" -> str("synthesized"))
  private def manifest(path: String): Value = obj("kind" -> str("manifest"), "path" -> str(path))
  private def withName(name: Value): Value  = obj("project" -> obj("name" -> name))
  private def withName(name: String): Value = withName(str(name))

  private def adHoc(
      entries: Chunk[(String, Value)] = files("src/Widget.elm" -> widget),
      root: String = "src",
      paths: Seq[String] = Seq("src/Widget.elm"),
      project: Value = synthesized,
      cliOverlay: Value = obj(),
      languageId: String = "elm",
      protocolVersion: Value = str("0.1.0-draft.1")
  ): Value = obj(
    "protocolVersion" -> protocolVersion,
    "developmentRoot" -> obj("entries" -> Structure.Value.Record(entries)),
    "morphirHome"     -> Structure.Value.Null,
    "systemConfig"    -> Structure.Value.Null,
    "environment"     -> obj(),
    "cliOverlay"      -> cliOverlay,
    "purpose"         -> obj(
      "kind"       -> str("ad-hoc-sources"),
      "project"    -> project,
      "sources"    -> obj("root" -> str(root), "paths" -> strs(paths*)),
      "languageId" -> str(languageId)
    )
  )

  private def fields(value: Value): Chunk[(String, Value)] = value match
    case Structure.Value.Record(fields) => fields
    case other                          => throw AssertionError(s"not a record: $other")

  private def updated(value: Value, key: String, field: Value): Value =
    val current = fields(value)
    if current.exists(_._1 == key) then
      Structure.Value.Record(current.map((name, old) => name -> (if name == key then field else old)))
    else Structure.Value.Record(current :+ (key -> field))

  private def without(value: Value, key: String): Value =
    Structure.Value.Record(fields(value).filterNot(_._1 == key))

  private def at(value: Value, path: String*): Option[Value] =
    path.foldLeft(Option(value)) {
      case (Some(Structure.Value.Record(fields)), field) => fields.iterator.toMap.get(field)
      case _                                             => None
    }

  private def discover(params: Value): Value =
    MepWorkspace.parseRequest(Some(params)) match
      case Right(request) => MepWorkspace.discover(request)
      case Left(message)  => throw AssertionError(s"params were refused as invalid: $message")

  private def failureOf(params: Value): (String, Value) =
    val response = discover(params)
    if !at(response, "status").contains(str("failure")) then
      throw AssertionError(s"expected a failure, got ${Json.encode(response)}")
    val code = at(response, "error", "code") match
      case Some(Structure.Value.Str(code)) => code
      case other                           => throw AssertionError(s"failure has no code: $other")
    if !at(response, "error", "message").exists(_.isInstanceOf[Structure.Value.Str]) then
      throw AssertionError(s"failure has no message: ${Json.encode(response)}")
    code -> at(response, "error", "path").get

  private def project(params: Value): Value =
    val response = discover(params)
    if !at(response, "status").contains(str("success")) then
      throw AssertionError(s"expected success, got ${Json.encode(response)}")
    at(response, "snapshot", "projects") match
      case Some(Structure.Value.Sequence(Chunk(project))) => project
      case other                                          => throw AssertionError(s"expected one project: $other")

  "MepWorkspace ad-hoc discovery" - {
    "names an unnamed single source for its declared module, on the wire" in {
      val response = discover(adHoc())

      assert(
        Json.encode(response) ==
          """{"status":"success","snapshot":{"protocolVersion":"0.1.0-draft.1","configAnchor":null,"name":null,""" +
          """"state":"open","projects":[{"name":"local/acme-widget","version":null,"relativePath":"src",""" +
          """"configAnchor":null,"sourceDirectory":".","state":"unloaded","diagnostics":[],""" +
          """"origin":{"kind":"synthesized","inputs":["src/Widget.elm"]},"exposedModules":["Acme.Widget"]}],""" +
          """"diagnostics":[]}}"""
      )
    }

    "names a port module, an effect module, and a header after comments like a plain module" in {
      val cases = Seq(
        (
          "port module App.Ports exposing (Size, sendMessage)\n\nport sendMessage : String -> Cmd msg\n",
          "local/app-ports",
          "App.Ports"
        ),
        (
          "effect module Foo.Bar where { command = MyCmd, subscription = MySub } exposing (Size)\n",
          "local/foo-bar",
          "Foo.Bar"
        ),
        ("{- outer {- nested -} comment -}\nmodule Acme.Widget exposing (Size)\n", "local/acme-widget", "Acme.Widget"),
        ("\uFEFF-- banner\nmodule Acme.Widget exposing (Size)\n", "local/acme-widget", "Acme.Widget"),
        ("module Foo_Bar exposing (X)\n", "local/foo_bar", "Foo_Bar")
      )

      cases.foreach { (text, name, module) =>
        val named = project(adHoc(entries = files("Source.elm" -> text), root = ".", paths = Seq("Source.elm")))
        assert(at(named, "name").contains(str(name)), s"for $text")
        assert(at(named, "exposedModules").contains(strs(module)), s"for $text")
      }
      succeed
    }

    "falls back to the file stem when the source declares no module" in {
      val named = project(adHoc(entries = files("src/Widget.elm" -> "x = 1\n")))

      assert(at(named, "name").contains(str("local/widget")))
      assert(at(named, "exposedModules").contains(strs("Widget")))
    }

    "falls back to Main when the file stem is not a module path" in {
      val named = project(
        adHoc(entries = files("src/not-a-module.elm" -> "x = 1\n"), paths = Seq("src/not-a-module.elm"))
      )

      assert(at(named, "name").contains(str("local/main")))
      assert(at(named, "exposedModules").contains(strs("Main")))
    }

    "keeps a trimmed explicit name and still derives exposure" in {
      val named = project(adHoc(cliOverlay = withName("  acme/widgets \n")))

      assert(at(named, "name").contains(str("acme/widgets")))
      assert(at(named, "exposedModules").contains(strs("Acme.Widget")))
    }

    "exposes every module of a named selection in selection order" in {
      val named = project(
        adHoc(
          entries = files("src/Widget.elm" -> widget, "src/Gadget.elm" -> gadget),
          paths = Seq("src/Widget.elm", "src/Gadget.elm"),
          cliOverlay = withName("acme/widgets")
        )
      )

      assert(at(named, "name").contains(str("acme/widgets")))
      assert(
        at(named, "origin").contains(
          obj("kind" -> str("synthesized"), "inputs" -> strs("src/Widget.elm", "src/Gadget.elm"))
        )
      )
      assert(at(named, "exposedModules").contains(strs("Acme.Widget", "Acme.Gadget")))
    }

    "takes a manifest selection's name from the overlay and its anchor from the manifest" in {
      val borrowed = project(
        adHoc(
          entries = files("morphir.toml" -> "", "src/Widget.elm" -> widget),
          project = manifest("morphir.toml"),
          cliOverlay = withName("acme/widgets")
        )
      )

      assert(
        borrowed == json(
          """{"name":"acme/widgets","version":null,"relativePath":"src","configAnchor":"morphir.toml",""" +
            """"sourceDirectory":".","state":"unloaded","diagnostics":[],""" +
            """"origin":{"kind":"manifest","path":"morphir.toml"},"exposedModules":["Acme.Widget"]}"""
        )
      )
    }

    "accepts every path under the mount root" in {
      val rooted = project(adHoc(entries = files("Widget.elm" -> widget), root = ".", paths = Seq("Widget.elm")))

      assert(at(rooted, "relativePath").contains(str(".")))
    }

    "does not validate a derived package name" in {
      val named = project(adHoc(entries = files("src/Widget.elm" -> "module ACME9.Widget exposing (x)\n")))

      assert(at(named, "name").contains(str("local/acme9-widget")))
    }

    "accepts a null CLI overlay and omitted optional fields" in {
      val request = without(without(without(adHoc(), "morphirHome"), "systemConfig"), "environment")

      assert(at(discover(updated(request, "cliOverlay", Structure.Value.Null)), "status").contains(str("success")))
      assert(at(discover(without(request, "cliOverlay")), "status").contains(str("success")))
    }

    "refuses each selection the contract rejects, with its code and path" in {
      val homeSymlink      = updated(adHoc(), "morphirHome", obj("entries" -> obj("a" -> symlink("b"))))
      val manifestProjects = updated(adHoc(), "purpose", obj("kind" -> str("manifest-projects")))
      val cases: Seq[(String, Value, String, Value)] = Seq(
        (
          "a later draft of the protocol",
          adHoc(protocolVersion = str("0.1.0-draft.2")),
          "workspace.protocol.unsupported",
          Structure.Value.Null
        ),
        (
          "the release the draft leads to",
          adHoc(protocolVersion = str("0.1.0")),
          "workspace.protocol.unsupported",
          Structure.Value.Null
        ),
        (
          "another release line",
          adHoc(protocolVersion = str("1.0.0")),
          "workspace.protocol.unsupported",
          Structure.Value.Null
        ),
        (
          "a CLI overlay that is not an object",
          adHoc(cliOverlay = strs("x")),
          "workspace.config.invalid",
          Structure.Value.Null
        ),
        (
          "a symlink in the development root",
          adHoc(entries = files("src/Widget.elm" -> widget) :+ ("src/Link.elm" -> symlink("src/Widget.elm"))),
          "workspace.symlink.unsupported",
          str("src/Link.elm")
        ),
        ("a symlink in Morphir Home", homeSymlink, "workspace.symlink.unsupported", str("a")),
        ("a manifest-projects purpose", manifestProjects, "workspace.purpose.unsupported", Structure.Value.Null),
        (
          "a request without a purpose",
          without(adHoc(), "purpose"),
          "workspace.purpose.unsupported",
          Structure.Value.Null
        ),
        ("an empty language id", adHoc(languageId = ""), "workspace.language-id.empty", str("src")),
        (
          "a non-string explicit name",
          adHoc(cliOverlay = withName(Structure.Value.Integer(7))),
          "workspace.config.invalid",
          str("src")
        ),
        ("a blank explicit name", adHoc(cliOverlay = withName(" \t")), "workspace.project-name.empty", str("src")),
        (
          "a manifest selection without an explicit name",
          adHoc(
            entries = files("morphir.toml" -> "", "src/Widget.elm" -> widget),
            project = manifest("morphir.toml")
          ),
          "workspace.selection.name-required",
          str("morphir.toml")
        ),
        (
          "a manifest that is not a file",
          adHoc(project = manifest("morphir.toml"), cliOverlay = withName("acme/widgets")),
          "workspace.selection.invalid",
          str("morphir.toml")
        ),
        ("an empty selection", adHoc(paths = Seq.empty), "workspace.selection.empty", str("src")),
        (
          "a repeated path",
          adHoc(paths = Seq("src/Widget.elm", "src/Widget.elm")),
          "workspace.selection.duplicate",
          str("src/Widget.elm")
        ),
        (
          "a path in a sibling that shares the root's prefix",
          adHoc(entries = files("src-other/Widget.elm" -> widget), paths = Seq("src-other/Widget.elm")),
          "workspace.selection.outside-root",
          str("src-other/Widget.elm")
        ),
        (
          "the root itself",
          adHoc(root = "src/Widget.elm"),
          "workspace.selection.outside-root",
          str("src/Widget.elm")
        ),
        (
          "a path that is a directory",
          adHoc(entries = files() :+ ("src/Widget.elm" -> directory)),
          "workspace.selection.invalid",
          str("src/Widget.elm")
        ),
        ("a missing path", adHoc(entries = files()), "workspace.selection.invalid", str("src/Widget.elm")),
        (
          "an explicit name outside the Elm package contract",
          adHoc(cliOverlay = withName("Acme/Widgets")),
          "workspace.project-name.invalid",
          str("src")
        ),
        (
          "an unnamed selection of two sources",
          adHoc(
            entries = files("src/Widget.elm" -> widget, "src/Gadget.elm" -> gadget),
            paths = Seq("src/Widget.elm", "src/Gadget.elm")
          ),
          "workspace.selection.name-required",
          str("src")
        ),
        (
          "two sources that define the same module",
          adHoc(
            entries = files("src/Widget.elm" -> widget, "src/Copy.elm" -> widget),
            paths = Seq("src/Widget.elm", "src/Copy.elm"),
            cliOverlay = withName("acme/widgets")
          ),
          "workspace.selection.module-collision",
          str("src/Copy.elm")
        )
      )

      cases.foreach { (label, params, code, path) =>
        assert(failureOf(params) == (code -> path), label)
      }
      succeed
    }

    "applies the checks in contract order where two failures coexist" in {
      val symlinkOnly                         = Chunk("a" -> symlink("b"))
      val twoCopies                           = files("src/Widget.elm" -> widget, "src/Copy.elm" -> widget)
      val cases: Seq[(String, Value, String)] = Seq(
        (
          "protocol before overlay",
          adHoc(protocolVersion = str("9.0.0"), cliOverlay = Structure.Value.Integer(1)),
          "workspace.protocol.unsupported"
        ),
        (
          "overlay before symlink",
          adHoc(cliOverlay = str("x"), entries = symlinkOnly),
          "workspace.config.invalid"
        ),
        (
          "symlink before purpose",
          updated(adHoc(entries = symlinkOnly), "purpose", obj("kind" -> str("manifest-projects"))),
          "workspace.symlink.unsupported"
        ),
        (
          "language id before name",
          adHoc(languageId = "", cliOverlay = withName("")),
          "workspace.language-id.empty"
        ),
        (
          "blank name before name-required manifest",
          adHoc(project = manifest("morphir.toml"), cliOverlay = withName(" ")),
          "workspace.project-name.empty"
        ),
        (
          "manifest name before manifest file",
          adHoc(project = manifest("morphir.toml")),
          "workspace.selection.name-required"
        ),
        (
          "manifest file before empty selection",
          adHoc(project = manifest("morphir.toml"), cliOverlay = withName("acme/widgets"), paths = Seq.empty),
          "workspace.selection.invalid"
        ),
        (
          "duplicate before outside root",
          adHoc(paths = Seq("other/A.elm", "other/A.elm")),
          "workspace.selection.duplicate"
        ),
        (
          "outside root before not a file",
          adHoc(paths = Seq("src/Missing.elm", "other/A.elm")),
          "workspace.selection.outside-root"
        ),
        (
          "not a file before the package contract",
          adHoc(paths = Seq("src/Missing.elm"), cliOverlay = withName("Not Canonical")),
          "workspace.selection.invalid"
        ),
        (
          "package contract before module collision",
          adHoc(
            entries = twoCopies,
            paths = Seq("src/Widget.elm", "src/Copy.elm"),
            cliOverlay = withName("Not Canonical")
          ),
          "workspace.project-name.invalid"
        ),
        (
          "name required before module collision",
          adHoc(entries = twoCopies, paths = Seq("src/Widget.elm", "src/Copy.elm")),
          "workspace.selection.name-required"
        )
      )

      cases.foreach { (label, params, code) =>
        assert(failureOf(params)._1 == code, label)
      }
      succeed
    }

    "reports the first symlink in path order" in {
      val params = adHoc(entries = Chunk("src/b" -> symlink("x"), "src/a" -> symlink("x")))

      assert(failureOf(params) == ("workspace.symlink.unsupported" -> str("src/a")))
    }

    "reports the first module collision in selection order" in {
      val params = adHoc(
        entries = files("src/A.elm" -> gadget, "src/B.elm" -> widget, "src/C.elm" -> widget, "src/D.elm" -> gadget),
        paths = Seq("src/A.elm", "src/B.elm", "src/C.elm", "src/D.elm"),
        cliOverlay = withName("acme/widgets")
      )

      assert(failureOf(params) == ("workspace.selection.module-collision" -> str("src/C.elm")))
    }

    "writes the reference failure messages" in {
      val messages = Seq(
        adHoc(protocolVersion = str("0.1.0")) ->
          "unsupported workspace discovery protocol 0.1.0; supported version is 0.1.0-draft.1",
        adHoc(cliOverlay = withName("Acme/Widgets")) ->
          "project name `Acme/Widgets` is invalid: it is not a canonical Morphir package name",
        adHoc(
          entries = files("src/Widget.elm" -> widget, "src/Gadget.elm" -> gadget),
          paths = Seq("src/Widget.elm", "src/Gadget.elm")
        ) ->
          ("ad-hoc selection rooted at `src` selects 2 sources but has no explicit name; " +
            "an unnamed synthesized selection must select exactly one source"),
        adHoc(
          entries = files("src/Widget.elm" -> widget, "src/Copy.elm" -> widget),
          paths = Seq("src/Widget.elm", "src/Copy.elm"),
          cliOverlay = withName("acme/widgets")
        ) -> "selected sources `src/Widget.elm` and `src/Copy.elm` both define module `Acme.Widget`",
        adHoc(paths = Seq("other/A.elm", "src/Widget.elm", "other/B.elm")) ->
          "selected paths are not under selection root `src`: `other/A.elm`, `other/B.elm`",
        adHoc(entries = files(), paths = Seq("src/A.elm", "src/B.elm"), cliOverlay = withName("acme/widgets")) ->
          "selected paths do not resolve to files: `src/A.elm`, `src/B.elm`"
      )

      messages.foreach { (params, message) =>
        assert(at(discover(params), "error", "message").contains(str(message)))
      }
      succeed
    }
  }

  "MepWorkspace request parsing" - {
    "refuses a malformed request as invalid params" in {
      val purpose                     = at(adHoc(), "purpose").get
      val cases: Seq[(String, Value)] = Seq(
        "a non-object request"                           -> arr(),
        "a missing protocol version"                     -> without(adHoc(), "protocolVersion"),
        "the integer protocol version of earlier drafts" -> adHoc(protocolVersion = Structure.Value.Integer(1)),
        "a protocol version that is not SemVer"          -> adHoc(protocolVersion = str("1")),
        "a protocol version with a leading v"            -> adHoc(protocolVersion = str("v0.1.0-draft.1")),
        "a missing development root"                     -> without(adHoc(), "developmentRoot"),
        "an unknown entry kind"                          -> adHoc(entries = Chunk("." -> obj("kind" -> str("socket")))),
        "a file entry without text"                      -> adHoc(entries = Chunk("a" -> obj("kind" -> str("file")))),
        "an absolute entry path"                         -> adHoc(entries = files("/etc/passwd" -> "")),
        "an escaping entry path"                         -> adHoc(entries = files("../Widget.elm" -> "")),
        "an empty entry path segment"                    -> adHoc(entries = files("src//Widget.elm" -> "")),
        "a backslash entry path"                         -> adHoc(entries = files("src\\Widget.elm" -> "")),
        "a drive-letter entry path"                      -> adHoc(entries = files("C:Widget.elm" -> "")),
        "a symlink target outside the mount"             -> adHoc(entries = Chunk("a" -> symlink("../b"))),
        "a malformed Morphir Home"                       -> updated(adHoc(), "morphirHome", Structure.Value.Integer(1)),
        "a non-string environment value" -> updated(adHoc(), "environment", obj("A" -> Structure.Value.Integer(1))),
        "an unknown purpose"             -> updated(adHoc(), "purpose", obj("kind" -> str("everything"))),
        "an unknown project source"      -> adHoc(project = obj("kind" -> str("remote"))),
        "a manifest without a path"      -> adHoc(project = obj("kind" -> str("manifest"))),
        "an escaping selection root"     -> adHoc(root = ".."),
        "an escaping selected path"      -> adHoc(paths = Seq("src/../Widget.elm")),
        "a non-string selected path"     -> updated(
          adHoc(),
          "purpose",
          updated(purpose, "sources", obj("root" -> str("src"), "paths" -> arr(Structure.Value.Integer(1))))
        ),
        "a missing language id" -> updated(adHoc(), "purpose", without(purpose, "languageId"))
      )

      cases.foreach { (label, params) =>
        assert(MepWorkspace.parseRequest(Some(params)).isLeft, label)
      }
      assert(MepWorkspace.parseRequest(None).isLeft)
      succeed
    }

    "speaks only the exact draft it writes" in {
      assert(MepWorkspace.speaks(MepWorkspace.ProtocolVersion))
      assert(Seq("0.1.0-draft.2", "0.1.0", "0.1.1", "0.2.0-draft.1", "1.0.0").forall(!MepWorkspace.speaks(_)))
    }

    "reads a request from the JSON text a host sends" in {
      val text =
        """{"protocolVersion":"0.1.0-draft.1","developmentRoot":{"entries":{".":{"kind":"directory"},""" +
          """"Example.elm":{"kind":"file","text":"module Example exposing (add)\n"}}},"morphirHome":null,""" +
          """"systemConfig":null,"environment":{"HOME":"/home/me"},"cliOverlay":{},"purpose":{"kind":"ad-hoc-sources",""" +
          """"project":{"kind":"synthesized"},"sources":{"root":".","paths":["Example.elm"]},"languageId":"elm"}}"""

      val named = project(json(text))

      assert(at(named, "name").contains(str("local/example")))
      assert(at(named, "exposedModules").contains(strs("Example")))
    }
  }
end MepWorkspaceTests

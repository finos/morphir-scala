//| scalaVersion: 3.8.4
//| mainClass: KbApp
//| moduleDeps: [KbCheck.scala, KbScaffold.scala, KbRender.scala, KbIndex.scala, KbRefresh.scala]
//| mvnDeps:
//| - io.getkyo::kyo-case-app:1.0.0-RC5

/** `kb` — knowledge base management for the Morphir knowledge base under `kb/`.
  *
  * CLI parsing and execution go through kyo-case-app; file access and process execution through kyo's `Path` and
  * `Command`. Every command accepts `--json`, so output can be chained or consumed by an agent.
  *
  * Run it through the launcher in this directory:
  *
  * {{{
  * .claude/skills/kb/kb list
  * .claude/skills/kb/kb check --json
  * }}}
  */

import caseapp.*
import caseapp.core.app.CommandsEntryPoint
import kyo.*
import java.time.LocalDate

// ------------------------------------------------------------------ options

case class CommonOpts(
    @HelpMessage("Path to the knowledge base root (the directory holding bundles/). Auto-detected when omitted.")
    kb: Option[String] = None,
    @HelpMessage("Emit JSON instead of text, for chaining and for agent consumption")
    json: Boolean = false
)

case class ListOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Show concepts within this bundle (name or group/name)") bundle: Option[String] = None
)

case class ShowOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Concept path — bundle-relative (/x.md) or a path suffix") path: String,
    @HelpMessage("Bundle to resolve a bundle-relative path against") bundle: Option[String] = None,
    @HelpMessage("Also include the document body") body: Boolean = false
)

case class SearchOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Text to look for in titles, descriptions, tags and paths") query: Option[String] = None,
    @HelpMessage("Also search document bodies") body: Boolean = false,
    @HelpMessage("Filter by frontmatter type") `type`: Option[String] = None,
    @HelpMessage("Filter by tag (repeatable)") tag: List[String] = Nil,
    @HelpMessage("Filter by status") status: Option[String] = None,
    @HelpMessage("Restrict to one bundle") bundle: Option[String] = None,
    @HelpMessage("Use the SQLite index for full-text search — faster, and ranks by relevance") index: Boolean = false,
    @HelpMessage("Row limit when using --index") limit: Int = 20,
    @HelpMessage("Index database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None
)

case class CheckOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Reference checkout root for provenance checks (default: <repo>/.refs)") refs: Option[String] = None,
    @HelpMessage("Skip provenance checks against .refs/") noProvenance: Boolean = false,
    @HelpMessage("Include info-level findings") verbose: Boolean = false,
    @HelpMessage("Exit non-zero when warnings are present, not just errors") strict: Boolean = false,
    @HelpMessage("Write the report here instead of stdout (convention: under .dev/)") out: Option[String] = None
)

case class IndexOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None,
    @HelpMessage("Report the index's freshness instead of rebuilding it") status: Boolean = false
)

case class RefreshOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Report what would change without writing anything") dryRun: Boolean = false,
    @HelpMessage("Rebuild the SQLite index even when it is already up to date") force: Boolean = false,
    @HelpMessage("Append index entries for concepts no index links to") addMissing: Boolean = false,
    @HelpMessage("Index section to append missing entries under") section: String = "Orientation",
    @HelpMessage("Skip the markdown indexes") noMarkdown: Boolean = false,
    @HelpMessage("Skip the SQLite index") noDb: Boolean = false,
    @HelpMessage("Database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None
)

case class QueryOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("SQL to run. Read-only: SELECT, WITH, PRAGMA or EXPLAIN") sql: String,
    @HelpMessage("Database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None
)

case class NewBundleOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Bundle slug, e.g. morphir-ir-v5") name: String,
    @HelpMessage("Grouping directory under bundles/, e.g. morphir") group: Option[String] = None,
    @HelpMessage("Bundle title") title: String,
    @HelpMessage("One-sentence bundle description") description: String,
    @HelpMessage("OKF version to declare") okfVersion: String = "0.2",
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class AddConceptOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Target bundle (name or group/name)") bundle: String,
    @HelpMessage("Path within the bundle, e.g. naming.md or design/naming.md") path: String,
    @HelpMessage("OKF type — the one required frontmatter field") `type`: String,
    @HelpMessage("Concept title") title: String,
    @HelpMessage("One-sentence description") description: String,
    @HelpMessage("Tag (repeatable)") tag: List[String] = Nil,
    @HelpMessage("Lifecycle status: draft, stable or deprecated") status: Option[String] = None,
    @HelpMessage("Source URL (repeatable); use id=URL or id=URL=Title to name it") source: List[String] = Nil,
    @HelpMessage("Index section heading to file the entry under") section: String = "Orientation",
    @HelpMessage("Actor for the generated.by frontmatter, e.g. process:kb-seed") generatedBy: Option[String] = None,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

// -------------------------------------------------------------------- shared

object KbCli:

  /** Mill sets this to the script directory; it is the only reliable anchor inside the script sandbox. */
  def workspace: Path =
    Path(sys.env.getOrElse("MILL_WORKSPACE_ROOT", sys.props.getOrElse("user.dir", ".")))

  private def descend(base: Path, relPath: String): Path =
    relPath.split('/').filter(_.nonEmpty).foldLeft(base)(_ / _)

  def at(p: String): Path =
    if p.startsWith("/") then Path(p) else descend(workspace, p)

  /** Walks up from the skill directory looking for a directory holding `kb/bundles`. */
  def resolveKb(explicit: Option[String]): Path < (Sync & Abort[Throwable]) =
    explicit match
      case Some(p) => (at(p): Path < (Sync & Abort[Throwable]))
      case None =>
        def climb(parts: Seq[String]): Path < (Sync & Abort[Throwable]) =
          if parts.isEmpty then Abort.fail(RuntimeException("could not locate a kb/ directory — pass --kb"))
          else
            val here = Path(parts*)
            (here / "kb" / "bundles").exists.map {
              case true => (here / "kb": Path < (Sync & Abort[Throwable]))
              case false => climb(parts.dropRight(1))
            }
        climb(workspace.parts.toSeq)

  /** The index is derived state, so it lives under `.dev/` with the rest of the tooling output. */
  def defaultDb(kbRoot: Path): Path =
    Path((kbRoot.parts.dropRight(1) ++ Seq(".dev", "kb", "index.db"))*)

  def dbPath(explicit: Option[String], kbRoot: Path): Path =
    explicit.map(at).getOrElse(defaultDb(kbRoot))

  def today(explicit: Option[String]): LocalDate =
    explicit.map(LocalDate.parse).getOrElse(LocalDate.now())

  def requireBundle(kb: Kb, label: String): Bundle < Abort[Throwable] =
    kb.bundle(label) match
      case Some(b) => (b: Bundle < Abort[Throwable])
      case None => Abort.fail(RuntimeException(s"no bundle `$label`; known: ${kb.bundles.map(_.label).mkString(", ")}"))

  def emit(text: String, out: Option[String]): Unit < (Sync & Abort[Throwable]) =
    out match
      case None => Console.print(text)
      case Some(p) =>
        val path = at(p)
        path.write(text).andThen(Console.printLine(s"wrote ${KbPath.render(path)}"))

  /** `id=URL`, `id=URL=Title`, or a bare URL. */
  def parseSource(s: String): (Option[String], String, Option[String]) =
    s.split("=", 3).toList match
      case id :: url :: title :: Nil if url.startsWith("http") => (Some(id), url, Some(title))
      case id :: url :: Nil if url.startsWith("http") => (Some(id), url, None)
      case _ => (None, s, None)

  /** Reports the error and exits non-zero, rather than dumping a kyo stack trace at the user. */
  def fail(msg: String): Unit < Sync =
    Sync.defer {
      java.lang.System.err.println(s"error: $msg")
      java.lang.System.exit(1)
    }

// ------------------------------------------------------------------ commands

object KbApp extends CommandsEntryPoint:
  override def progName: String = "kb"
  def commands = Seq(ListCmd, ShowCmd, SearchCmd, CheckCmd, IndexCmd, RefreshCmd, QueryCmd, NewBundleCmd, AddConceptCmd)

  object ListCmd extends KyoCommand[ListOpts]:
    override def name = "list"
    run { (o: ListOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        text <- o.bundle match
          case None => (KbRender.listBundles(kb, o.common.json): String < (Sync & Abort[Throwable]))
          case Some(b) => KbCli.requireBundle(kb, b).map(KbRender.listConcepts(kb, _, o.common.json))
        _ <- Console.print(text)
      yield ()
    }

  object ShowCmd extends KyoCommand[ShowOpts]:
    override def name = "show"
    run { (o: ShowOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        _ <- Console.print(KbRender.show(kb, o.path, o.bundle, o.body, o.common.json))
      yield ()
    }

  object SearchCmd extends KyoCommand[SearchOpts]:
    override def name = "search"
    run { (o: SearchOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        _ <-
          if o.index then
            o.query match
              case None => KbCli.fail("--index needs --query")
              case Some(q) =>
                KbIndex.search(KbCli.dbPath(o.db, root), q, o.limit).map {
                  case Left(err) => KbCli.fail(err)
                  case Right(rows) => Console.print(KbIndex.renderRows(rows, o.common.json))
                }
          else
            KbStore.load(root).map { kb =>
              Console.print(KbRender.search(kb, o.query, o.body, o.`type`, o.tag, o.status, o.bundle, o.common.json))
            }
      yield ()
    }

  object IndexCmd extends KyoCommand[IndexOpts]:
    override def name = "index"
    run { (o: IndexOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        db = KbCli.dbPath(o.db, root)
        _ <-
          if o.status then
            KbIndex.status(db, kb).map {
              case Left(err) => KbCli.fail(err)
              case Right((builtAt, docs, stale)) =>
                if o.common.json then
                  Console.print(ujson.write(
                    ujson.Obj(
                      "db" -> KbPath.render(db),
                      "builtAt" -> builtAt,
                      "docs" -> docs,
                      "staleCount" -> stale.size,
                      "stale" -> ujson.Arr.from(stale.map(ujson.Str(_)))
                    ),
                    indent = 2
                  ) + "\n")
                else
                  val head = s"index ${KbPath.render(db)}\n  built $builtAt over $docs doc(s)\n"
                  val tail =
                    if stale.isEmpty then "  up to date\n"
                    else s"  ${stale.size} file(s) changed since — rerun `kb index`\n" + stale.map("    " + _ + "\n").mkString
                  Console.print(head + tail)
            }
          else
            KbIndex.build(kb, db).map(s => Console.print(KbIndex.renderStats(s, db, o.common.json)))
      yield ()
    }

  object RefreshCmd extends KyoCommand[RefreshOpts]:
    override def name = "refresh"
    run { (o: RefreshOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        md <-
          if o.noMarkdown then (Seq.empty[RefreshAction]: Seq[RefreshAction] < (Sync & Abort[Throwable]))
          else KbRefresh.refreshMarkdown(kb, o.addMissing, o.section, o.dryRun)
        // Reload after rewriting the markdown so the index is built from what is now on disk.
        reloaded <- if md.isEmpty || o.dryRun then (kb: Kb < (Sync & Abort[Throwable])) else KbStore.load(root)
        dbActions <-
          if o.noDb then (Seq.empty[RefreshAction]: Seq[RefreshAction] < (Sync & Abort[Throwable]))
          else KbRefresh.refreshDb(reloaded, KbCli.dbPath(o.db, root), o.force, o.dryRun)
        _ <- Console.print(KbRefresh.render(md ++ dbActions, o.dryRun, o.common.json))
      yield ()
    }

  object QueryCmd extends KyoCommand[QueryOpts]:
    override def name = "query"
    run { (o: QueryOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        res <- KbIndex.query(KbCli.dbPath(o.db, root), o.sql)
        _ <- res match
          case Left(err) => KbCli.fail(err)
          case Right(rows) => Console.print(KbIndex.renderRows(rows, o.common.json))
      yield ()
    }

  object CheckCmd extends KyoCommand[CheckOpts]:
    override def name = "check"
    run { (o: CheckOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        refsCandidate = o.refs.map(KbCli.at).getOrElse(Path((root.parts.dropRight(1) :+ ".refs")*))
        refsPresent <- if o.noProvenance then (false: Boolean < (Sync & Abort[Throwable])) else refsCandidate.exists
        findings <- KbCheck.run(kb, Option.when(refsPresent)(refsCandidate), LocalDate.now())
        text = if o.common.json then KbCheck.renderJson(findings) else KbCheck.renderText(findings, o.verbose)
        _ <- KbCli.emit(text, o.out)
        errs = findings.count(_.severity == Severity.Error)
        warns = findings.count(_.severity == Severity.Warn)
        _ <-
          if errs > 0 || (o.strict && warns > 0) then Sync.defer(java.lang.System.exit(1))
          else ((): Unit < (Sync & Abort[Throwable]))
      yield ()
    }

  object NewBundleCmd extends KyoCommand[NewBundleOpts]:
    override def name = "new-bundle"
    run { (o: NewBundleOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        res <- KbScaffold.newBundle(root, o.name, o.group, o.title, o.description, o.okfVersion, KbCli.today(o.date))
        _ <- res match
          case Left(err) => KbCli.fail(err)
          case Right(r) => Console.print(KbRender.scaffold(r, o.common.json))
      yield ()
    }

  object AddConceptCmd extends KyoCommand[AddConceptOpts]:
    override def name = "add-concept"
    run { (o: AddConceptOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        bundle <- KbCli.requireBundle(kb, o.bundle)
        res <- KbScaffold.addConcept(
          bundle = bundle,
          relPath = o.path,
          conceptType = o.`type`,
          title = o.title,
          description = o.description,
          tags = o.tag,
          status = o.status,
          sources = o.source.map(KbCli.parseSource),
          section = o.section,
          generatedBy = o.generatedBy,
          today = KbCli.today(o.date)
        )
        _ <- res match
          case Left(err) => KbCli.fail(err)
          case Right(r) => Console.print(KbRender.scaffold(r, o.common.json))
      yield ()
    }

//| scalaVersion: 3.8.4
//| mainClass: KbApp
//| moduleDeps: [KbCheck.scala, KbScaffold.scala, KbRender.scala, KbIndex.scala, KbRefresh.scala, KbIntentEdit.scala, KbSync.scala]
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
    @HelpMessage("Report dangling links as warnings — OKF's stance that they mark not-yet-written knowledge") allowDangling: Boolean = false,
    @HelpMessage("Write the report here instead of stdout (convention: under .dev/)") out: Option[String] = None
)

case class IndexOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None,
    @HelpMessage("Report the index's freshness instead of rebuilding it") status: Boolean = false
)

/** `kb refresh` — everything. The `--no-*` flags narrow it; the `refresh markdown` and `refresh db` subcommands do
  * the same thing more legibly.
  */
case class RefreshOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Report what would change without writing anything") dryRun: Boolean = false,
    @HelpMessage("Rebuild the SQLite index even when it is already up to date") force: Boolean = false,
    @HelpMessage("Append index entries for concepts no index links to") addMissing: Boolean = false,
    @HelpMessage("Index section to append missing entries under") section: String = "Orientation",
    @HelpMessage("Skip the markdown indexes — same as `kb refresh db`") noMarkdown: Boolean = false,
    @HelpMessage("Skip the SQLite index — same as `kb refresh markdown`") noDb: Boolean = false,
    @HelpMessage("Database path (default: <repo>/.dev/kb/index.db)") db: Option[String] = None
)

/** `kb refresh markdown` — index bullets only. */
case class RefreshMarkdownOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Report what would change without writing anything") dryRun: Boolean = false,
    @HelpMessage("Append index entries for concepts no index links to") addMissing: Boolean = false,
    @HelpMessage("Index section to append missing entries under") section: String = "Orientation"
)

/** `kb refresh db` — the SQLite index only. */
case class RefreshDbOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Report what would change without writing anything") dryRun: Boolean = false,
    @HelpMessage("Rebuild even when the index is already up to date") force: Boolean = false,
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

// ---------------------------------------------------------------- intent opts

case class IntentInitOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Bundle name under bundles/") name: String = "intent",
    @HelpMessage("Package URL identifying the system, e.g. pkg:maven/org.finos.morphir/morphir-core") system: Option[String] = None,
    @HelpMessage("Bundle label holding capabilities, e.g. morphir/morphir-scala") capabilityBundle: Option[String] = None,
    @HelpMessage("Days before an active intent is reported stale") staleAfterDays: Int = 60,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class IntentNewOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("What the work is") title: String,
    @HelpMessage("One sentence — also the index entry") description: String,
    @HelpMessage("feature, bug, performance, security, deprecation, removal, refactor, docs, test, build, spike") kind: String,
    @HelpMessage("Marks a compatibility break — orthogonal to kind") breaking: Boolean = false,
    @HelpMessage("GitHub issue number this came from") issue: Option[String] = None,
    @HelpMessage("Tag (repeatable)") tag: List[String] = Nil,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class IntentListOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Filter by state") state: Option[String] = None,
    @HelpMessage("Filter by kind") kind: Option[String] = None,
    @HelpMessage("Only breaking intent") breaking: Boolean = false,
    @HelpMessage("Only open intent — excludes Released, Cancelled, Superseded") open: Boolean = false,
    @HelpMessage("Only user-visible kinds, as release notes would show") userVisible: Boolean = false
)

case class IntentShowOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Intent id or slug, e.g. 0007") id: Option[String] = None
)

case class IntentCheckOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Exit non-zero on warnings too") strict: Boolean = false,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

/** Obligation-free transitions: refine, start, and the generic escape hatch. */
case class IntentMoveOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Intent id or slug") id: Option[String] = None,
    @HelpMessage("Target state (move only)") state: Option[String] = None,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class IntentReleaseOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Intent id or slug") id: Option[String] = None,
    @HelpMessage("Capability this produced, as bundle-label:/path.md") capability: Option[String] = None,
    @HelpMessage("Package URL of a shipped artifact (repeatable)") artifact: List[String] = Nil,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class IntentCancelOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Intent id or slug") id: Option[String] = None,
    @HelpMessage("Why the work is not being done") reason: Option[String] = None,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class IntentSupersedeOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Intent id or slug") id: Option[String] = None,
    @HelpMessage("Intent id that replaces it") by: Option[String] = None,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class DecisionListOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Filter by state") state: Option[String] = None,
    @HelpMessage("Only decisions that still govern — excludes Superseded and Withdrawn") inForce: Boolean = false,
    @HelpMessage("Restrict to one bundle") bundle: Option[String] = None
)

case class DecisionShowOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Decision id or slug, e.g. 0004") id: Option[String] = None,
    @HelpMessage("Bundle to look in — required when an id means a record in more than one") bundle: Option[String] = None,
    @HelpMessage("Also include the document body") body: Boolean = false
)

case class SyncCommonOpts(
    @Recurse common: CommonOpts = CommonOpts(),
    @HelpMessage("Bundle to sync (defaults to the one whose index declares `sync: true`)") bundle: Option[String] = None,
    @HelpMessage("Reference checkouts root (default: <repo>/.refs)") refs: Option[String] = None
)

case class SyncStatusOpts(
    @Recurse sync: SyncCommonOpts = SyncCommonOpts(),
    @HelpMessage("Do not consult the upstream checkout — compare the mirror against the lockfile only") noUpstream: Boolean = false,
    @HelpMessage("List clean files too") verbose: Boolean = false,
    @HelpMessage("Exit non-zero when anything has diverged") strict: Boolean = false
)

case class SyncPullOpts(
    @Recurse sync: SyncCommonOpts = SyncCommonOpts(),
    @HelpMessage("Report what would change without writing anything") dryRun: Boolean = false,
    @HelpMessage("Take upstream's version of files that changed on both sides") theirs: Boolean = false,
    @HelpMessage("Delete mirrored files that upstream has removed") prune: Boolean = false,
    @HelpMessage("Override today's date (YYYY-MM-DD)") date: Option[String] = None
)

case class SyncPushOpts(
    @Recurse sync: SyncCommonOpts = SyncCommonOpts(),
    @HelpMessage("Checkout to write the upstream form into (default: the reference checkout)") to: Option[String] = None,
    @HelpMessage("Report what would be written without writing anything") dryRun: Boolean = false,
    @HelpMessage("Also export files that changed upstream since the last import") includeDiverged: Boolean = false
)

case class SyncDiffOpts(
    @Recurse sync: SyncCommonOpts = SyncCommonOpts(),
    @HelpMessage("Mirrored path, e.g. docs/spec/draft/types.md") path: Option[String] = None
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

  /** The one refresh implementation. `kb refresh`, `kb refresh markdown` and `kb refresh db` differ only in which
    * halves they enable, so they all land here.
    */
  def runRefresh(
      kbOpt: Option[String],
      json: Boolean,
      markdown: Boolean,
      database: Boolean,
      dryRun: Boolean,
      force: Boolean,
      addMissing: Boolean,
      section: String,
      dbOpt: Option[String]
  ): Unit < (Sync & Abort[Throwable]) =
    for
      root <- resolveKb(kbOpt)
      kb <- KbStore.load(root)
      md <-
        if !markdown then (Seq.empty[RefreshAction]: Seq[RefreshAction] < (Sync & Abort[Throwable]))
        else KbRefresh.refreshMarkdown(kb, addMissing, section, dryRun)
      // Reload after rewriting the markdown so the database is built from what is now on disk.
      reloaded <- if md.isEmpty || dryRun then (kb: Kb < (Sync & Abort[Throwable])) else KbStore.load(root)
      dbActions <-
        if !database then (Seq.empty[RefreshAction]: Seq[RefreshAction] < (Sync & Abort[Throwable]))
        else KbRefresh.refreshDb(reloaded, dbPath(dbOpt, root), force, dryRun)
      _ <- Console.print(KbRefresh.render(md ++ dbActions, dryRun, json))
    yield ()

  /** Loads the kb and locates the intent bundle, failing with guidance when there is none. */
  def withIntent[A](kbOpt: Option[String])(f: (Kb, Bundle) => A < (Sync & Abort[Throwable])): A < (Sync & Abort[Throwable]) =
    for
      root <- resolveKb(kbOpt)
      kb <- KbStore.load(root)
      out <- KbIntent.findBundle(kb) match
        case Some(b) => f(kb, b)
        case None =>
          Abort.fail(RuntimeException(
            "no intent bundle — no bundle index declares `intent: true`. Run `kb intent init` to scaffold one."
          ))
    yield out

  /** Record ids read better as positionals (`kb intent start 0007`, `kb decision show 0004`) than as `--id`. */
  def recordId(flag: Option[String], rest: caseapp.RemainingArgs): Option[String] =
    flag.orElse(rest.remaining.headOption).map(_.trim).filter(_.nonEmpty)

  /** `.refs/` sits beside `kb/`, which is the convention `kb check` already follows for provenance. */
  def refsRoot(explicit: Option[String], kbRoot: Path): Path =
    explicit.map(at).getOrElse(Path((kbRoot.parts.dropRight(1) :+ ".refs")*))

  /** Loads the kb and the sync bundle's manifest and lockfile, failing with guidance when either is missing. */
  def withSync[A](o: SyncCommonOpts)(
      f: (Path, Kb, SyncBundle) => A < (Async & Abort[Throwable])
  ): A < (Async & Abort[Throwable]) =
    for
      root <- resolveKb(o.common.kb)
      kb <- KbStore.load(root)
      b <- KbSync.findBundle(kb, o.bundle) match
        case Some(b) => (b: Bundle < (Async & Abort[Throwable]))
        case None =>
          Abort.fail(RuntimeException(
            "no sync bundle — no bundle index declares `sync: true`. Pass --bundle, or add a sync.yaml."
          ))
      loaded <- KbSync.load(b)
      out <- loaded match
        case Left(err) => Abort.fail(RuntimeException(err))
        case Right(sb) => f(root, kb, sb)
    yield out

  /** The upstream checkout a sync bundle reads from, when it is present on disk. */
  def upstreamRoot(refs: Path, sb: SyncBundle): Option[Path] < (Sync & Abort[Throwable]) =
    val candidate = sb.manifest.refsPath.split("/").filter(_.nonEmpty).foldLeft(refs)(_ / _)
    candidate.exists.map(Option.when(_)(candidate))

  /** Sync findings for every bundle that mirrors something, folded into `kb check` alongside the rest. */
  def allSyncFindings(kb: Kb, refs: Path, useUpstream: Boolean): Seq[Finding] < (Sync & Abort[Throwable]) =
    Kyo.foreach(Chunk.from(kb.bundles.filter(_.mirror.isDefined))) { b =>
      KbSync.load(b).map {
        case Left(_) => (Chunk.empty[Finding]: Chunk[Finding] < (Sync & Abort[Throwable]))
        case Right(sb) =>
          val up = if useUpstream then upstreamRoot(refs, sb) else (None: Option[Path] < (Sync & Abort[Throwable]))
          up.map(KbSync.checkFindings(kb, sb, _).map(Chunk.from))
      }
    }.map(_.flattenChunk.toSeq)

  def requireUpstream(refs: Path, sb: SyncBundle): Path < (Async & Abort[Throwable]) =
    upstreamRoot(refs, sb).map {
      case Some(p) => (p: Path < (Async & Abort[Throwable]))
      case None =>
        Abort.fail(RuntimeException(
          s"no reference checkout for ${sb.manifest.repo} under ${KbPath.render(refs)} — " +
            s"add one with `/squire reference repo add https://github.com/${sb.manifest.repo}`"
        ))
    }

  def runTransition(
      kbOpt: Option[String],
      json: Boolean,
      id: String,
      dateOpt: Option[String]
  )(build: Intent => KbIntentEdit.Transition): Unit < (Sync & Abort[Throwable]) =
    withIntent(kbOpt) { (kb, b) =>
      KbIntent.find(b, id) match
        case None => fail(s"no intent `$id` in ${b.label}")
        case Some(i) =>
          val t = build(i)
          KbIntentEdit.transition(kb, b, i, t, today(dateOpt)).map {
            case Left(err) => fail(err)
            case Right(file) =>
              if json then
                Console.print(ujson.write(
                  ujson.Obj("id" -> i.id, "state" -> t.to.toString, "file" -> KbPath.render(file)),
                  indent = 2
                ) + "\n")
              else
                Console.printLine(s"intent ${i.id} → ${t.to}")
                  .andThen(Console.printLine(s"  ${kb.rel(file)}"))
                  .andThen(Console.printLine("  run `kb refresh` to regenerate the intent index"))
          }
    }

  /** Reports the error and exits non-zero, rather than dumping a kyo stack trace at the user. */
  def fail(msg: String): Unit < Sync =
    Sync.defer {
      java.lang.System.err.println(s"error: $msg")
      java.lang.System.exit(1)
    }

// ------------------------------------------------------------------ commands

object KbApp extends CommandsEntryPoint:
  override def progName: String = "kb"
  def commands =
    Seq(ListCmd, ShowCmd, SearchCmd, CheckCmd, IndexCmd, RefreshCmd, RefreshMarkdownCmd, RefreshDbCmd, QueryCmd,
      NewBundleCmd, AddConceptCmd,
      SyncStatusCmd, SyncPullCmd, SyncPushCmd, SyncDiffCmd,
      IntentInitCmd, IntentNewCmd, IntentListCmd, IntentShowCmd, IntentCheckCmd,
      IntentRefineCmd, IntentStartCmd, IntentMoveCmd, IntentReleaseCmd, IntentCancelCmd, IntentSupersedeCmd,
      DecisionListCmd, DecisionShowCmd)

  object DecisionListCmd extends KyoCommand[DecisionListOpts]:
    override def name = "decision list"
    override def names = List(List("decision", "list"), List("decision", "ls"))
    run { (o: DecisionListOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        kb <- KbStore.load(root)
        wanted = o.state.flatMap(DecisionState.parse)
        items = KbDecision.decisions(kb).filter { d =>
          wanted.forall(s => d.state.contains(s)) &&
          o.bundle.forall(b => d.bundle == b || d.bundle.endsWith(s"/$b")) &&
          (!o.inForce || d.state.forall(!_.isRetired))
        }
        _ <- Console.print(KbDecision.renderList(items, o.common.json))
      yield ()
    }

  object DecisionShowCmd extends KyoCommand[DecisionShowOpts]:
    override def name = "decision show"
    override def names = List(List("decision", "show"))
    run { (o: DecisionShowOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give a decision id, e.g. `kb decision show 0004`")
        case Some(id) =>
          for
            root <- KbCli.resolveKb(o.common.kb)
            kb <- KbStore.load(root)
            _ <- KbDecision.findAll(kb, id, o.bundle) match
              case Seq() => KbCli.fail(s"no decision record `$id`")
              case Seq(d) => Console.print(KbDecision.renderShow(d, o.body, o.common.json))
              // Ids are unique per bundle, so the same number can name a different decision in each. Picking one
              // would be silent and wrong; say which bundles have it and let the caller name one.
              case many => KbCli.fail(
                  s"`$id` names a decision record in ${many.size} bundles — pass --bundle to choose:\n" +
                    many.map(d => s"  ${d.bundle}  ${d.slug}").mkString("\n")
                )
          yield ()
    }

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
      KbCli.runRefresh(
        o.common.kb, o.common.json,
        markdown = !o.noMarkdown, database = !o.noDb,
        o.dryRun, o.force, o.addMissing, o.section, o.db
      )
    }

  object RefreshMarkdownCmd extends KyoCommand[RefreshMarkdownOpts]:
    override def name = "refresh markdown"
    override def names = List(List("refresh", "markdown"), List("refresh", "md"))
    run { (o: RefreshMarkdownOpts) =>
      KbCli.runRefresh(
        o.common.kb, o.common.json,
        markdown = true, database = false,
        o.dryRun, force = false, o.addMissing, o.section, None
      )
    }

  object RefreshDbCmd extends KyoCommand[RefreshDbOpts]:
    override def name = "refresh db"
    override def names = List(List("refresh", "db"), List("refresh", "index"))
    run { (o: RefreshDbOpts) =>
      KbCli.runRefresh(
        o.common.kb, o.common.json,
        markdown = false, database = true,
        o.dryRun, o.force, addMissing = false, section = "Orientation", o.db
      )
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
        core <- KbCheck.run(kb, Option.when(refsPresent)(refsCandidate), LocalDate.now(), o.allowDangling)
        sync <- KbCli.allSyncFindings(kb, refsCandidate, refsPresent)
        findings = (core ++ sync).sortBy(f => (f.severity.ordinal, f.path, f.line.getOrElse(0)))
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

  // ------------------------------------------------------------------ intent

  object IntentInitCmd extends KyoCommand[IntentInitOpts]:
    override def name = "intent init"
    override def names = List(List("intent", "init"))
    run { (o: IntentInitOpts) =>
      for
        root <- KbCli.resolveKb(o.common.kb)
        res <- KbIntentEdit.initBundle(root, o.name, o.system, o.capabilityBundle, o.staleAfterDays, KbCli.today(o.date))
        _ <- res match
          case Left(err) => KbCli.fail(err)
          case Right(files) =>
            Console.print(KbRender.scaffold(ScaffoldResult(files, Nil, Seq(
              s"add the bundle to the Bundles table in ${KbPath.render(root / "README.md")}"
            )), o.common.json))
      yield ()
    }

  object IntentNewCmd extends KyoCommand[IntentNewOpts]:
    override def name = "intent new"
    override def names = List(List("intent", "new"))
    run { (o: IntentNewOpts) =>
      KbCli.withIntent(o.common.kb) { (kb, b) =>
        IntentKind.parse(o.kind) match
          case None => KbCli.fail(s"unknown kind `${o.kind}` — one of ${IntentKind.names}")
          case Some(k) =>
            KbIntentEdit.create(b, o.title, o.description, k, o.breaking, o.issue, o.tag, KbCli.today(o.date)).map {
              case Left(err) => KbCli.fail(err)
              case Right(file) =>
                if o.common.json then
                  Console.print(ujson.write(ujson.Obj("file" -> KbPath.render(file)), indent = 2) + "\n")
                else
                  Console.printLine(s"created ${kb.rel(file)}")
                    .andThen(Console.printLine("  write the Problem section, then `kb refresh`"))
            }
      }
    }

  object IntentListCmd extends KyoCommand[IntentListOpts]:
    override def name = "intent list"
    override def names = List(List("intent", "list"), List("intent", "ls"))
    run { (o: IntentListOpts) =>
      KbCli.withIntent(o.common.kb) { (kb, b) =>
        val wanted = o.state.flatMap(IntentState.parse)
        val wantedKind = o.kind.flatMap(IntentKind.parse)
        val items = KbIntent.intents(b).filter { i =>
          wanted.forall(s => i.state.contains(s)) &&
          wantedKind.forall(k => i.kind.contains(k)) &&
          (!o.breaking || i.breaking) &&
          (!o.open || i.state.forall(!_.isTerminal)) &&
          (!o.userVisible || i.kind.exists(_.userVisible))
        }
        Console.print(KbIntent.renderList(kb, b, items, o.common.json))
      }
    }

  object IntentShowCmd extends KyoCommand[IntentShowOpts]:
    override def name = "intent show"
    override def names = List(List("intent", "show"))
    run { (o: IntentShowOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id, e.g. `kb intent show 0007`")
        case Some(id) =>
          KbCli.withIntent(o.common.kb) { (kb, b) =>
            KbIntent.find(b, id) match
              case None => KbCli.fail(s"no intent `$id` in ${b.label}")
              case Some(i) => Console.print(KbIntent.renderShow(kb, i, o.common.json))
          }
    }

  object IntentCheckCmd extends KyoCommand[IntentCheckOpts]:
    override def name = "intent check"
    override def names = List(List("intent", "check"))
    run { (o: IntentCheckOpts) =>
      KbCli.withIntent(o.common.kb) { (kb, b) =>
        val findings = KbIntent.check(kb, b, KbCli.today(o.date))
        val text = if o.common.json then KbCheck.renderJson(findings) else KbCheck.renderText(findings, verbose = true)
        val errs = findings.count(_.severity == Severity.Error)
        val warns = findings.count(_.severity == Severity.Warn)
        Console.print(text).andThen(
          if errs > 0 || (o.strict && warns > 0) then Sync.defer(java.lang.System.exit(1))
          else ((): Unit < (Sync & Abort[Throwable]))
        )
      }
    }

  object IntentRefineCmd extends KyoCommand[IntentMoveOpts]:
    override def name = "intent refine"
    override def names = List(List("intent", "refine"))
    run { (o: IntentMoveOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id")
        case Some(id) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ => KbIntentEdit.Transition(IntentState.Refinement))
    }

  object IntentStartCmd extends KyoCommand[IntentMoveOpts]:
    override def name = "intent start"
    override def names = List(List("intent", "start"))
    run { (o: IntentMoveOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id")
        case Some(id) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ => KbIntentEdit.Transition(IntentState.InProgress))
    }

  object IntentMoveCmd extends KyoCommand[IntentMoveOpts]:
    override def name = "intent move"
    override def names = List(List("intent", "move"))
    run { (o: IntentMoveOpts, rest: caseapp.RemainingArgs) =>
      (KbCli.recordId(o.id, rest), o.state.flatMap(IntentState.parse)) match
        case (None, _) => KbCli.fail("give an intent id")
        case (_, None) => KbCli.fail(s"--state must be one of ${IntentState.all.mkString(", ")}")
        case (Some(id), Some(target)) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ => KbIntentEdit.Transition(target))
    }

  object IntentReleaseCmd extends KyoCommand[IntentReleaseOpts]:
    override def name = "intent release"
    override def names = List(List("intent", "release"))
    run { (o: IntentReleaseOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id")
        case Some(id) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ =>
            KbIntentEdit.Transition(IntentState.Released, capability = o.capability, artifacts = o.artifact))
    }

  object IntentCancelCmd extends KyoCommand[IntentCancelOpts]:
    override def name = "intent cancel"
    override def names = List(List("intent", "cancel"))
    run { (o: IntentCancelOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id")
        case Some(id) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ =>
            KbIntentEdit.Transition(IntentState.Cancelled, reason = o.reason))
    }

  object IntentSupersedeCmd extends KyoCommand[IntentSupersedeOpts]:
    override def name = "intent supersede"
    override def names = List(List("intent", "supersede"))
    run { (o: IntentSupersedeOpts, rest: caseapp.RemainingArgs) =>
      KbCli.recordId(o.id, rest) match
        case None => KbCli.fail("give an intent id")
        case Some(id) =>
          KbCli.runTransition(o.common.kb, o.common.json, id, o.date)(_ =>
            KbIntentEdit.Transition(IntentState.Superseded, supersededBy = o.by))
    }

  // ------------------------------------------------------------------- sync

  object SyncStatusCmd extends KyoCommand[SyncStatusOpts]:
    override def name = "sync status"
    override def names = List(List("sync", "status"))
    run { (o: SyncStatusOpts) =>
      KbCli.withSync(o.sync) { (root, _, sb) =>
        val refs = KbCli.refsRoot(o.sync.refs, root)
        for
          up <- if o.noUpstream then (None: Option[Path] < (Sync & Abort[Throwable])) else KbCli.upstreamRoot(refs, sb)
          rows <- KbSync.status(sb, up)
          _ <- Console.print(KbSync.renderStatus(rows, o.sync.common.json, o.verbose))
          bad = rows.count(r => r.state == SyncState.Diverged || r.state == SyncState.Unreadable)
          _ <-
            if o.strict && bad > 0 then Sync.defer(java.lang.System.exit(1))
            else ((): Unit < (Sync & Abort[Throwable]))
        yield ()
      }
    }

  object SyncPullCmd extends KyoCommand[SyncPullOpts]:
    override def name = "sync pull"
    override def names = List(List("sync", "pull"))
    run { (o: SyncPullOpts) =>
      KbCli.withSync(o.sync) { (root, _, sb) =>
        val refs = KbCli.refsRoot(o.sync.refs, root)
        val today = KbCli.today(o.date)
        for
          up <- KbCli.requireUpstream(refs, sb)
          head <- KbCheck.gitHead(up)
          result <- KbSync.pull(sb, up, head.getOrElse(sb.lock.baseCommit), today, o.dryRun, o.theirs, o.prune)
          _ <-
            if o.dryRun then ((): Unit < (Async & Abort[Throwable]))
            else
              for
                written <- KbSync.writeLock(sb, result.lock)
                // Reload before generating the index: the bullets have to carry the descriptions of the concepts
                // that were just written, and the in-memory bundle predates them.
                reloaded <- KbStore.load(root)
                _ <- KbSync.findBundle(reloaded, o.sync.bundle) match
                  case Some(b) => KbSync.generateIndex(sb.copy(bundle = b), written, today).unit
                  case None => ((): Unit < (Sync & Abort[Throwable]))
              yield ()
          _ <- Console.print(KbSync.renderActions(result.actions, result.refused, o.dryRun, o.sync.common.json))
          // Non-zero when anything was held back, matching `sync push`. Without it a caller that only checks the
          // exit code — spec-sync.py does — reports a clean import over files that were never imported.
          _ <-
            if result.refused.nonEmpty then Sync.defer(java.lang.System.exit(1))
            else ((): Unit < (Sync & Abort[Throwable]))
        yield ()
      }
    }

  object SyncPushCmd extends KyoCommand[SyncPushOpts]:
    override def name = "sync push"
    override def names = List(List("sync", "push"))
    run { (o: SyncPushOpts) =>
      KbCli.withSync(o.sync) { (root, _, sb) =>
        val refs = KbCli.refsRoot(o.sync.refs, root)
        for
          target <- o.to match
            case Some(p) => (KbCli.at(p): Path < (Async & Abort[Throwable]))
            case None => KbCli.requireUpstream(refs, sb)
          // Consult the checkout even when exporting elsewhere: it is the only way to know whether a change here
          // would be overwriting a change there.
          up <- KbCli.upstreamRoot(refs, sb)
          out <- KbSync.push(sb, target, up, o.dryRun, o.includeDiverged)
          (actions, refused) = out
          _ <- Console.print(KbSync.renderActions(actions, refused, o.dryRun, o.sync.common.json))
          _ <-
            if refused.nonEmpty then Sync.defer(java.lang.System.exit(1))
            else ((): Unit < (Sync & Abort[Throwable]))
        yield ()
      }
    }

  object SyncDiffCmd extends KyoCommand[SyncDiffOpts]:
    override def name = "sync diff"
    override def names = List(List("sync", "diff"))
    run { (o: SyncDiffOpts, rest: caseapp.RemainingArgs) =>
      o.path.orElse(rest.remaining.headOption).map(_.trim).filter(_.nonEmpty) match
        case None => KbCli.fail("give a mirrored path, e.g. `kb sync diff docs/spec/draft/types.md`")
        case Some(rel) =>
          KbCli.withSync(o.sync) { (root, _, sb) =>
            val refs = KbCli.refsRoot(o.sync.refs, root)
            for
              up <- KbCli.requireUpstream(refs, sb)
              local <- KbSync.readBytes(sb.localFile(rel))
              _ <- KbSync.project(String(local, "UTF-8")) match
                case Left(err) => KbCli.fail(s"$rel: $err")
                case Right(projected) =>
                  // Diff the *upstream form* of our copy, which is the thing an export would send.
                  val tmp = Path(sys.props.getOrElse("java.io.tmpdir", "/tmp")) / s"kb-sync-${rel.replace('/', '_')}"
                  for
                    _ <- KbSync.writeBytes(tmp, projected.getBytes("UTF-8"))
                    res <- Abort.run[Throwable](
                      Command(
                        "git", "diff", "--no-index", "--",
                        KbPath.render(KbSync.resolve(up, rel)),
                        KbPath.render(tmp)
                      ).textWithExitCode
                    )
                    _ <- res match
                      case Result.Success((text, _)) if text.trim.isEmpty => Console.printLine(s"$rel: identical")
                      case Result.Success((text, _)) => Console.print(text)
                      case _ => KbCli.fail("git diff failed — is git on PATH?")
                  yield ()
            yield ()
          }
    }

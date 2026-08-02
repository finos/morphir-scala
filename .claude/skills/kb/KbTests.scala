//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [KbIntentEdit.scala, KbRefresh.scala, KbRender.scala, KbSync.scala]
//| mvnDeps:
//| - io.getkyo::kyo-test-api:1.0.0-RC5
//| - io.getkyo::kyo-test-runner:1.0.0-RC5

/** Tests for the kb and intent skill code, using kyo-test — the same framework `langkit` and `kit` use.
  *
  * Mill's script mode exposes no test module (`mill test <script>` does not resolve), so the suite runs through
  * kyo-test's own CLI runner as the script's `mainClass`. Suites are discovered by ServiceLoader, which is why
  * `test-resources/META-INF/services/kyo.test.Test` lists them — **add a new suite there or it will never run.**
  *
  * {{{
  * mise run kb:test
  * }}}
  *
  * Cases marked *regression* pin behaviour that was once wrong. Three came out of the Codex review on #936 and one
  * out of dogfooding; none would have been caught by the manual checking that preceded them.
  */

import kyo.*
import kyo.test.*
import java.time.LocalDate

/** Fixtures shared across the suites. Nothing here touches the real knowledge base. */
object KbFixtures:
  val today: LocalDate = LocalDate.parse("2026-07-28")

  def scratch(label: String): Path < (Sync & Abort[Throwable]) =
    Sync.defer(Path(java.nio.file.Files.createTempDirectory(s"kb-test-$label-").toAbsolutePath.toString))

  /** A minimal knowledge base: one ordinary bundle, optionally an intent bundle alongside it. */
  def fixture(withIntent: Boolean): Path < (Sync & Abort[Throwable]) =
    for
      root <- scratch("kb")
      kbRoot = root / "kb"
      _ <- KbScaffold.newBundle(kbRoot, "demo", None, "Demo", "A scratch bundle.", "0.2", today)
      _ <-
        if !withIntent then ((): Unit < (Sync & Abort[Throwable]))
        else KbIntentEdit.initBundle(kbRoot, "intent", Some("pkg:pypi/demo"), Some("demo"), 60, today).map(_ => ())
    yield kbRoot

  /** A knowledge base with one intent already created, returned loaded. */
  def withOneIntent(title: String, kind: IntentKind): (Path, Kb, Bundle, Intent) < (Sync & Abort[Throwable]) =
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, title, "Something.", kind, false, None, Nil, today)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
    yield (kbRoot, kb, b, KbIntent.find(b, "0001").get)

import KbFixtures.*

// ---------------------------------------------------------------------- parsing

class KbParsingSpec extends Test[Any]:

  "splitFrontmatter" - {
    "returns None when there is no fence" in {
      val (fm, body) = KbStore.splitFrontmatter("# Title\n\nprose\n")
      assert(fm.isEmpty && body.contains("# Title"))
    }
    "separates a normal block" in {
      val (fm, body) = KbStore.splitFrontmatter("---\ntype: Concept\n---\n\n# Title\n")
      assert(fm.contains("type: Concept\n") && body.contains("# Title") && !body.contains("type:"))
    }
    "normalises CRLF" in {
      val (fm, _) = KbStore.splitFrontmatter("---\r\ntype: Concept\r\n---\r\n\r\nbody\r\n")
      assert(fm.contains("type: Concept\n"))
    }
    "treats an unterminated fence as no frontmatter" in {
      val (fm, body) = KbStore.splitFrontmatter("---\ntype: Concept\n\nnever closed\n")
      assert(fm.isEmpty && body.contains("never closed"))
    }
  }

  "parseFrontmatter" - {
    "rejects duplicate keys" in {
      assert(KbStore.parseFrontmatter("type: A\ntype: B\n").isLeft)
    }
    "rejects a document that is not a mapping" in {
      assert(KbStore.parseFrontmatter("- just\n- a list\n").left.exists(_.contains("expected a mapping")))
    }
    "regression: reads an unquoted date as a string rather than as absent" in {
      // SnakeYAML resolves `2026-07-28` to a java.util.Date. Without explicit handling, every date-valued field —
      // OKF's stale_after, intent's created and state_since — silently read as None, and the checks never fired.
      val fm = KbStore.parseFrontmatter("type: Intent\ncreated: 2026-07-28\n").toOption.get
      assert(fm.str("created").contains("2026-07-28"))
    }
    "reads tags and nested sources" in {
      val fm = KbStore
        .parseFrontmatter("type: Concept\ntags: [a, b]\nsources:\n  - id: s1\n    resource: https://example.com/x.md\n")
        .toOption
        .get
      assert(fm.tags == List("a", "b"))
      assert(fm.sources.size == 1 && fm.sources.head.id.contains("s1"))
      assert(fm.sources.head.resource == "https://example.com/x.md")
    }
  }

  "markdown" - {
    "reports link line numbers offset past the frontmatter" in {
      val links = KbStore.extractLinks("intro\n\n[one](/a.md) and [two](https://x)\n", frontmatterLines = 5)
      assert(links.map(_.dest) == Seq("/a.md", "https://x"))
      assert(links.head.line == 8)
      assert(links.head.isBundleRelative && links(1).isExternal)
    }
    "ignores links inside fenced code" in {
      val links = KbStore.extractLinks("```\n[not a link](/nope.md)\n```\n\n[real](/yes.md)\n", 0)
      assert(links.map(_.dest) == Seq("/yes.md"))
    }
    "skips headings inside fenced code" in {
      val hs = KbIndex.headings("# Real\n\n```bash\n# just a shell comment\n```\n\n## Also real\n")
      assert(hs.map(_._2) == Seq("Real", "Also real"))
    }
  }

// ------------------------------------------------------------------------ paths

class KbPathsSpec extends Test[Any]:

  "segmentsUnder" - {
    "relativises a path below the base" in {
      assert(KbPath.segmentsUnder(Path("/a/b/c/d.md"), Path("/a/b")).contains(Seq("c", "d.md")))
    }
    "refuses an unrelated path" in {
      assert(KbPath.segmentsUnder(Path("/x/y"), Path("/a/b")).isEmpty)
    }
    "isUnder agrees" in {
      assert(KbPath.isUnder(Path("/a/b/c"), Path("/a/b")) && !KbPath.isUnder(Path("/x"), Path("/a/b")))
    }
  }

  "DocRef" - {
    "parses bundle:path" in {
      assert(DocRef.parse("morphir/morphir-scala:/x.md").contains(DocRef("morphir/morphir-scala", "/x.md")))
    }
    "rejects a missing colon" in { assert(DocRef.parse("no-colon").isEmpty) }
    "rejects a path that is not bundle-relative" in { assert(DocRef.parse("bundle:relative.md").isEmpty) }
    "rejects a Package URL — the two schemes are deliberately distinct" in {
      assert(DocRef.parse("pkg:maven/org/x@1.0").isEmpty)
    }
  }

  "vocabulary" - {
    "parses hyphenated states" in { assert(IntentState.parse("in-progress").contains(IntentState.InProgress)) }
    "rejects an unknown state" in { assert(IntentState.parse("nonsense").isEmpty) }
    "knows which states are terminal" in { assert(IntentState.Released.isTerminal && !IntentState.Backlog.isTerminal) }
    "never treats Backlog as active — a backlog is meant to sit" in { assert(!IntentState.Backlog.isActive) }
    "carries the user-visible tier on the kind" in {
      assert(IntentKind.parse("feature").exists(_.userVisible))
      assert(IntentKind.parse("build").exists(!_.userVisible))
    }
    "slugifies to kebab-case" in { assert(KbScaffold.slugify("  Release Labels, v2! ") == "release-labels-v2") }
  }

// --------------------------------------------------------------------- scaffold

class KbScaffoldSpec extends Test[Any]:

  "frontmatter editing" - {
    "regression: appends after a trailing block rather than inside it" in {
      // Anchoring on "the last top-level line" put the new key between `sources:` and its children, corrupting the
      // YAML and the provenance with it. Found by review, not by manual checking.
      for
        dir <- scratch("setkeys")
        f = dir / "doc.md"
        _ <- f.write("---\ntype: Intent\nstate: Backlog\nsources:\n  - id: s1\n    resource: https://x/y.md\n---\n\nbody\n")
        _ <- KbIntentEdit.setKeys(f, Seq("capability" -> Some("demo:/cap.md")))
        text <- f.read
      yield
        val fm = KbStore.parseFrontmatter(KbStore.splitFrontmatter(text)._1.getOrElse("")).toOption
        assert(fm.isDefined, s"frontmatter no longer parses:\n$text")
        assert(fm.get.str("capability").contains("demo:/cap.md"))
        assert(fm.get.sources.size == 1 && fm.get.sources.head.resource == "https://x/y.md")
    }
    "replaces an existing key in place and leaves the body alone" in {
      for
        dir <- scratch("setkeys2")
        f = dir / "doc.md"
        _ <- f.write("---\ntype: Intent\nstate: Backlog\n---\n\n# Body\n\nprose\n")
        _ <- KbIntentEdit.setKeys(f, Seq("state" -> Some("Released")))
        text <- f.read
      yield assert(text.contains("state: Released") && !text.contains("state: Backlog") && text.contains("# Body"))
    }
  }

  "add-concept" - {
    "regression: refuses a path that escapes the bundle" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb <- KbStore.load(kbRoot)
        b = kb.bundle("demo").get
        res <- KbScaffold.addConcept(b, "../escaped.md", "Concept", "X", "Y.", Nil, None, Nil, "Orientation", None, today)
        leaked <- (kbRoot / "bundles" / "escaped.md").exists
      yield
        assert(res.left.exists(_.contains("must stay inside the bundle")), s"expected refusal, got $res")
        assert(!leaked, "nothing may be written outside the bundle")
    }
    "refuses the reserved OKF filenames" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb <- KbStore.load(kbRoot)
        b = kb.bundle("demo").get
        res <- KbScaffold.addConcept(b, "index.md", "Concept", "X", "Y.", Nil, None, Nil, "Orientation", None, today)
      yield assert(res.left.exists(_.contains("reserved")))
    }
    "writes the concept and wires up the index and log" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb <- KbStore.load(kbRoot)
        b = kb.bundle("demo").get
        _ <- KbScaffold.addConcept(b, "naming.md", "Concept", "Naming", "How things are named.", Seq("x"), Some("draft"), Nil, "Orientation", None, today)
        idx <- b.index.file.read
        log <- b.log.get.file.read
        concept <- (b.root / "naming.md").read
      yield
        assert(idx.contains("[Naming](/naming.md) - How things are named."), "index entry mirrors the description")
        assert(log.contains("**Creation**"))
        assert(concept.contains("type: Concept") && concept.contains("status: draft"))
    }
  }

  "log entries" - {
    "create a date section once, then append within it" in {
      for
        dir <- scratch("log")
        f = dir / "log.md"
        _ <- f.write("# Log\n")
        _ <- KbScaffold.appendLogEntry(f, today, "**Creation**: one.")
        _ <- KbScaffold.appendLogEntry(f, today, "**Update**: two.")
        text <- f.read
      yield
        assert(text.linesIterator.count(_.startsWith(s"## $today")) == 1, "one date heading, not two")
        assert(text.contains("* **Creation**: one.") && text.contains("* **Update**: two."))
    }
  }

// ----------------------------------------------------------------------- intent

class KbIntentSpec extends Test[Any]:

  "discovery" - {
    "finds the intent bundle by its marker, not by its path" in {
      for
        kbRoot <- fixture(withIntent = true)
        kb <- KbStore.load(kbRoot)
      yield
        val b = KbIntent.findBundle(kb)
        assert(b.isDefined, "intent bundle not found")
        val cfg = KbIntent.config(b.get)
        assert(cfg.system.contains("pkg:pypi/demo"))
        assert(cfg.capabilityBundle.contains("demo") && cfg.staleAfterDays == 60)
    }
    "allocates ids sequentially" in {
      for (_, _, b, _) <- withOneIntent("First", IntentKind.Feature)
      yield
        assert(KbIntent.nextId(b) == "0002")
        assert(KbIntent.find(b, "1").map(_.id).contains("0001"), "findable by bare number")
    }
  }

  "releasing" - {
    "demands a capability for user-visible kinds" in {
      for
        (_, kb, b, i) <- withOneIntent("Feature work", IntentKind.Feature)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released), today)
      yield assert(res.left.exists(_.contains("needs --capability")), s"expected refusal, got $res")
    }
    "rejects a malformed capability reference" in {
      for
        (_, kb, b, i) <- withOneIntent("Feature work", IntentKind.Feature)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released, capability = Some("nope")), today)
      yield assert(res.left.exists(_.contains("is not `bundle-label:/path.md`")), s"expected refusal, got $res")
    }
    "rejects a capability reference that resolves to nothing" in {
      for
        (_, kb, b, i) <- withOneIntent("Feature work", IntentKind.Feature)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released, capability = Some("demo:/missing.md")), today)
      yield assert(res.left.exists(_.contains("names no concept")), s"expected refusal, got $res")
    }
    "needs no capability for internal kinds" in {
      // Internal work often changes nothing a reader of the knowledge base needs to know; inventing a document for
      // "added three release labels" would be exactly the noise the design avoids. See docs/adr/0001.
      for
        (_, kb, b, i) <- withOneIntent("Build work", IntentKind.Build)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released), today)
      yield assert(res.isRight, s"internal kinds may release without a capability, got $res")
    }
  }

  "other terminal states" - {
    "cancelling demands a reason" in {
      for
        (_, kb, b, i) <- withOneIntent("A thing", IntentKind.Feature)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Cancelled), today)
      yield assert(res.left.exists(_.contains("needs --reason")), s"expected refusal, got $res")
    }
    "superseding demands a successor that exists" in {
      for
        (_, kb, b, i) <- withOneIntent("A thing", IntentKind.Feature)
        res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Superseded, supersededBy = Some("0099")), today)
      yield assert(res.left.exists(_.contains("no intent")), s"expected refusal, got $res")
    }
  }

  "check" - {
    "reports a released intent with no capability" in {
      for
        (kbRoot, _, _, i) <- withOneIntent("Stuck", IntentKind.Feature)
        // Hand-edited into an impossible state, to exercise the checker rather than the transition guard.
        _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state" -> Some("Released")))
        kb <- KbStore.load(kbRoot)
      yield
        val findings = KbIntent.check(kb, KbIntent.findBundle(kb).get, today)
        assert(
          findings.exists(f => f.check == "intent-released-no-capability" && f.severity == Severity.Error),
          s"got ${findings.map(_.check)}"
        )
    }
    "warns when active work has not moved" in {
      for
        (kbRoot, _, _, i) <- withOneIntent("Stuck", IntentKind.Feature)
        _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state" -> Some("Refinement"), "state_since" -> Some("2026-01-01")))
        kb <- KbStore.load(kbRoot)
      yield
        val findings = KbIntent.check(kb, KbIntent.findBundle(kb).get, today)
        assert(findings.exists(f => f.check == "intent-stale" && f.severity == Severity.Warn), s"got ${findings.map(_.check)}")
    }
    "never calls a backlog item stale" in {
      for
        (kbRoot, _, _, i) <- withOneIntent("Waiting", IntentKind.Feature)
        _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state_since" -> Some("2026-01-01")))
        kb <- KbStore.load(kbRoot)
      yield assert(!KbIntent.check(kb, KbIntent.findBundle(kb).get, today).exists(_.check == "intent-stale"))
    }
  }

  "generated index" - {
    "groups by state and keeps the preamble" in {
      for
        (_, _, b, _) <- withOneIntent("Open thing", IntentKind.Feature)
        _ <- KbIntentEdit.generateIndex(b, today)
        text <- b.index.file.read
      yield
        assert(text.contains(KbIntent.Marker), "marker retained")
        assert(text.contains("## Backlog (1)"), "grouped heading with a count")
        assert(text.contains("- Something."), "entry description is verbatim")
        // Flags belong in the link text, never after the description — otherwise kb check sees drift.
        assert(!text.contains("- Something. _("), "no flags appended after the description")
    }
  }

// ------------------------------------------------------------------------ check

class KbCheckSpec extends Test[Any]:

  "links" - {
    "report as an error, and downgrade under --allow-dangling" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb0 <- KbStore.load(kbRoot)
        b0 = kb0.bundle("demo").get
        _ <- KbScaffold.addConcept(b0, "a.md", "Concept", "A", "First.", Nil, None, Nil, "Orientation", None, today)
        _ <- (b0.root / "a.md").read.map(t => (b0.root / "a.md").write(t + "\nSee [missing](/gone.md).\n"))
        kb <- KbStore.load(kbRoot)
        strict <- KbCheck.run(kb, None, today)
        lenient <- KbCheck.run(kb, None, today, allowDangling = true)
      yield
        assert(strict.exists(f => f.check == "link-broken" && f.severity == Severity.Error), s"got ${strict.map(_.check)}")
        assert(lenient.exists(f => f.check == "link-broken" && f.severity == Severity.Warn), "downgraded, not suppressed")
    }
  }

  "concepts" - {
    "report a missing type and an unindexed concept" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb0 <- KbStore.load(kbRoot)
        b0 = kb0.bundle("demo").get
        _ <- (b0.root / "orphan.md").write("---\ntitle: Orphan\n---\n\nNo type, nobody links to it.\n")
        kb <- KbStore.load(kbRoot)
        findings <- KbCheck.run(kb, None, today)
      yield
        assert(findings.exists(_.check == "concept-missing-type"))
        assert(findings.exists(_.check == "concept-not-indexed"))
    }
  }

  "index drift" - {
    "is detected, and repaired by refresh" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb0 <- KbStore.load(kbRoot)
        b0 = kb0.bundle("demo").get
        _ <- KbScaffold.addConcept(b0, "a.md", "Concept", "A", "The real description.", Nil, None, Nil, "Orientation", None, today)
        _ <- b0.index.file.read.map(t => b0.index.file.write(t.replace("The real description.", "stale text")))
        kb <- KbStore.load(kbRoot)
        before <- KbCheck.run(kb, None, today)
        actions <- KbRefresh.refreshMarkdown(kb, addMissing = false, section = "Orientation", dryRun = false)
        kb2 <- KbStore.load(kbRoot)
        after <- KbCheck.run(kb2, None, today)
      yield
        assert(before.exists(_.check == "index-description-drift"), "drift detected")
        assert(actions.exists(_.kind == RefreshKind.DescriptionFixed), "refresh reports the fix")
        assert(!after.exists(_.check == "index-description-drift"), "drift gone after refresh")
    }
    "a dry run reports without writing" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb0 <- KbStore.load(kbRoot)
        b0 = kb0.bundle("demo").get
        _ <- KbScaffold.addConcept(b0, "a.md", "Concept", "A", "Real description.", Nil, None, Nil, "Orientation", None, today)
        _ <- b0.index.file.read.map(t => b0.index.file.write(t.replace("Real description.", "stale")))
        kb <- KbStore.load(kbRoot)
        before <- b0.index.file.read
        actions <- KbRefresh.refreshMarkdown(kb, addMissing = false, section = "Orientation", dryRun = true)
        after <- b0.index.file.read
      yield assert(actions.nonEmpty && after == before, "dry run must not write")
    }
  }

// ------------------------------------------------------------------------ index

class KbIndexSpec extends Test[Any]:

  "building" - {
    "records documents, links and frontmatter facets" in {
      for
        (kbRoot, _, _, _) <- withOneIntent("Indexed thing", IntentKind.Feature)
        kb <- KbStore.load(kbRoot)
        db = kbRoot / "index.db"
        stats <- KbIndex.build(kb, db)
        rows <- KbIndex.query(db, "SELECT state, kind FROM v_intent")
      yield
        assert(stats.docs > 0, "documents indexed")
        val r = rows.toOption.get
        assert(r.rows.size == 1, s"one intent row, got ${r.rows.size}")
        assert(r.rows.head.head.contains("Backlog"), "state pivoted out of the generic frontmatter table")
        assert(r.rows.head(1).contains("feature"), "kind pivoted")
    }
  }

  "querying" - {
    "refuses anything that is not read-only" in {
      for
        kbRoot <- fixture(withIntent = false)
        kb <- KbStore.load(kbRoot)
        db = kbRoot / "index.db"
        _ <- KbIndex.build(kb, db)
        res <- KbIndex.query(db, "DELETE FROM doc")
      yield assert(res.left.exists(_.contains("read-only")), s"expected refusal, got $res")
    }
  }

  "staleness" - {
    "regression: notices a document deleted since the build" in {
      // Staleness compared modification times, and a deleted file has none — so the index kept serving a document
      // that no longer existed, and `refresh db` skipped the rebuild. Found by review.
      for
        kbRoot <- fixture(withIntent = false)
        kb0 <- KbStore.load(kbRoot)
        b0 = kb0.bundle("demo").get
        _ <- KbScaffold.addConcept(b0, "doomed.md", "Concept", "Doomed", "About to go.", Nil, None, Nil, "Orientation", None, today)
        kb <- KbStore.load(kbRoot)
        db = kbRoot / "index.db"
        _ <- KbIndex.build(kb, db)
        fresh <- KbIndex.status(db, kb)
        _ <- (b0.root / "doomed.md").remove
        kb2 <- KbStore.load(kbRoot)
        after <- KbIndex.status(db, kb2)
      yield
        assert(fresh.toOption.get._3.isEmpty, "fresh immediately after building")
        assert(after.toOption.get._3.exists(_.contains("removed since")), s"got ${after.toOption.get._3}")
    }
  }

// ------------------------------------------------------------------------- sync

/** The vendoring engine, and above all its one load-bearing invariant.
  *
  * `project(inject(bytes)) == bytes` is what makes an export safe. If it slips, the knowledge base silently rewrites
  * somebody else's repository — so the corpus below is deliberately hostile: CRLF, nested YAML blocks, fractional
  * numbers, `---` in the body, and no frontmatter at all.
  */
class KbSyncSpec extends Test[Any]:

  /** Documents that have all previously been plausible ways to break the projection. */
  private val corpus: Seq[(String, String)] = Seq(
    "plain frontmatter" -> "---\ntitle: Types\ndescription: The type system\n---\n\n# Types\n\nProse.\n",
    "no frontmatter at all" -> "# Agents\n\nA working agreement.\n",
    "CRLF throughout" -> "---\r\ntitle: Types\r\n---\r\n\r\n# Types\r\n",
    "nested block" -> "---\ntitle: IR v4\nstatus: partial\ntracking:\n  beads: [morphir-8fx]\n  github_issues: [398]\n---\n\nBody.\n",
    "fractional sidebar_position" -> "---\ntitle: Attributes\nsidebar_position: 2.5\n---\n\nBody.\n",
    "body contains a fence" -> "---\ntitle: X\n---\n\nBefore.\n\n---\n\nAfter.\n",
    "empty frontmatter block" -> "---\n---\n\nBody.\n",
    "no trailing newline" -> "---\ntitle: X\n---\n\nBody without a newline",
    "unterminated frontmatter" -> "---\ntitle: never closed\n\n# Body\n"
  )

  private val keys = Seq("type" -> "Specification Source", "kb_upstream" -> "docs/spec/draft/types.md")

  "the round-trip invariant" - {
    corpus.foreach { (label, text) =>
      s"survives $label" in {
        val injected = KbSync.inject(text, keys)
        assert(KbSync.project(injected) == Right(text), s"round-trip changed the bytes for $label")
      }
    }
    "injects keys that actually parse as YAML" in {
      val injected = KbSync.inject(corpus.head._2, keys)
      val fm = KbSync.split(injected).flatMap(s => KbStore.parseFrontmatter(s.fm).toOption).get
      assert(fm.`type`.contains("Specification Source"), s"got ${fm.`type`}")
      assert(fm.title.contains("Types"), "upstream's own keys must survive injection")
    }
    "leaves a document that was never injected alone" in {
      assert(KbSync.project(corpus.head._2) == Right(corpus.head._2))
    }
    "removes the whole block when upstream had no frontmatter" in {
      val injected = KbSync.inject("# Agents\n\nProse.\n", keys)
      assert(injected.startsWith("---\n"), "a block is added to carry the keys")
      assert(KbSync.project(injected) == Right("# Agents\n\nProse.\n"))
    }
    "refuses a damaged fence rather than guessing" in {
      val broken = "---\ntitle: X\n# kb:begin\ntype: Y\n---\n\nBody.\n"
      assert(KbSync.project(broken).isLeft, "an unclosed fence must not silently export")
    }
  }

  "injectedKeys" - {
    "supplies title and description only when upstream omits them" in {
      val m = KbSync.parseManifest("upstream:\n  repo: acme/spec\nmappings:\n  - docs/**\n").toOption.get
      val withBoth = KbSync.injectedKeys(m, "docs/a.md", "---\ntitle: A\ndescription: B\n---\n")
      val without = KbSync.injectedKeys(m, "docs/some-file.md", "# No frontmatter\n")
      assert(withBoth.map(_._1) == Seq("type", "kb_upstream"), s"got ${withBoth.map(_._1)}")
      assert(without.map(_._1) == Seq("type", "title", "description", "kb_upstream"), s"got ${without.map(_._1)}")
      assert(without.toMap.get("title").contains("Some File"), s"got ${without.toMap.get("title")}")
    }
  }

  "relative links in mirrored documents" - {
    "regression: a sibling link resolves to an absolute path that exists" in {
      // resolveLink rebuilt the path from a segment list, dropping the leading empty segment of an absolute path
      // along with `.` and `""`. Every relative link then resolved to a relative path that could not exist. Authored
      // concepts link bundle-relative, so nothing noticed until a mirror full of upstream's relative links arrived.
      for
        (kbRoot, _, upstream) <- syncFixture
        _ <- KbSync.writeBytes(
          upstream / "docs" / "guide.md",
          "---\ntitle: Guide\ndescription: A guide.\n---\n\nSee [types](types.md) and [gone](missing.md).\n".getBytes("UTF-8")
        )
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        kb <- KbStore.load(kbRoot)
        b = kb.bundle("vendored").get
        guide = b.concepts.find(_.name == "guide.md").get
        resolved = guide.links.flatMap(l => KbStore.resolveLink(guide, l))
        existence <- Kyo.foreach(Chunk.from(resolved))(p => p.exists.map(p -> _))
      yield
        assert(resolved.size == 2, s"got ${resolved.map(KbPath.render)}")
        assert(
          existence.exists((p, ok) => ok && KbPath.render(p).endsWith("/sources/docs/types.md")),
          s"got ${existence.map((p, ok) => KbPath.render(p) -> ok)}"
        )
        assert(existence.exists((p, ok) => !ok && KbPath.render(p).endsWith("missing.md")), "and a real miss still misses")
    }
  }

  "glob matching" - {
    "** spans directories and also matches zero of them" in {
      assert(KbGlob.matches("docs/**", "docs/spec/draft/types.md"))
      assert(KbGlob.matches("docs/**/x.md", "docs/x.md"), "**/ must match zero directories")
      assert(!KbGlob.matches("docs/**", "wit/types.wit"))
    }
    "* stops at a separator" in {
      assert(KbGlob.matches("website/static/schemas/morphir-*.yaml", "website/static/schemas/morphir-ir-v4.yaml"))
      assert(!KbGlob.matches("docs/*.md", "docs/spec/types.md"))
    }
    "a dot is literal, not any-character" in {
      assert(!KbGlob.matches("docs/a.md", "docs/axmd"))
    }
  }

  "the manifest" - {
    "needs a repo and at least one mapping" in {
      assert(KbSync.parseManifest("mappings:\n  - docs/**\n").isLeft, "no upstream.repo")
      assert(KbSync.parseManifest("upstream:\n  repo: a/b\n").isLeft, "no mappings")
    }
    "resolves type by first matching glob, in declaration order" in {
      val m = KbSync.parseManifest(
        "upstream:\n  repo: a/b\nmappings:\n  - docs/**\ntype_map:\n  \"docs/design/**\": Design Source\n  \"docs/**\": Specification Source\n"
      ).toOption.get
      assert(m.typeFor("docs/design/ir/values.md") == "Design Source")
      assert(m.typeFor("docs/spec/types.md") == "Specification Source")
      assert(m.typeFor("wit/x.wit") == "Source Document", "unmatched paths fall back")
    }
    "selects by mapping minus exclusions" in {
      val m = KbSync.parseManifest(
        "upstream:\n  repo: a/b\nmappings:\n  - { from: \"website/**\", exclude: [\"**/*.json\"] }\nexclude:\n  - \"**/big.yaml\"\n"
      ).toOption.get
      assert(m.selects("website/static/schemas/x.yaml"))
      assert(!m.selects("website/static/schemas/x.json"), "mapping-level exclude")
      assert(!m.selects("website/static/schemas/big.yaml"), "manifest-level exclude")
    }
    "classifies markdown as a concept and everything else as an asset" in {
      val m = KbSync.parseManifest("upstream:\n  repo: a/b\nmappings:\n  - \"**\"\n").toOption.get
      assert(m.kindOf("docs/x.md") == SyncKind.Concept)
      // .mdx carries JSX, which commonmark has no business parsing.
      assert(m.kindOf("docs/x.mdx") == SyncKind.Asset)
      assert(m.kindOf("schemas/x.yaml") == SyncKind.Asset)
    }
  }

  "the lockfile" - {
    "round-trips through render and parse" in {
      val lock = SyncLock("abc123", "2026-07-28", Seq(
        LockEntry("docs/b.md", SyncKind.Concept, "hash-b"),
        LockEntry("wit/a.wit", SyncKind.Asset, "hash-a")
      ))
      val rendered = KbSync.renderLock(lock)
      val back = KbSync.parseLock(rendered).toOption.get
      assert(back.baseCommit == "abc123")
      assert(back.get("wit/a.wit").map(_.kind).contains(SyncKind.Asset))
      // Sorted on write, so a pull that changes nothing produces no diff.
      assert(rendered.indexOf("docs/b.md") < rendered.indexOf("wit/a.wit"), "entries are written in path order")
    }
  }

  // ------------------------------------------------------------ end to end

  /** A knowledge base with one sync bundle, plus a fake upstream checkout to pull from. */
  private def syncFixture: (Path, Path, Path) < (Sync & Abort[Throwable]) =
    for
      root <- scratch("sync")
      kbRoot = root / "kb"
      upstream = root / "upstream"
      _ <- KbScaffold.newBundle(kbRoot, "vendored", None, "Vendored", "Mirrored upstream material.", "0.2", today)
      bundleRoot = kbRoot / "bundles" / "vendored"
      _ <- KbIntentEdit.setKeys(bundleRoot / "index.md", Seq("sync" -> Some("true")))
      _ <- (bundleRoot / "sync.yaml").write(
        "upstream:\n  repo: acme/spec\n  refs_path: acme/spec\nroot: sources\n" +
          "mappings:\n  - \"docs/**\"\n  - \"schemas/**\"\ntype_map:\n  \"docs/**\": Specification Source\n"
      )
      _ <- KbSync.writeBytes(upstream / "docs" / "types.md", "---\ntitle: Types\ndescription: The types.\n---\n\n# Types\n".getBytes("UTF-8"))
      _ <- KbSync.writeBytes(upstream / "docs" / "index.md", "---\ntitle: Docs\ndescription: The docs.\n---\n\n# Docs\n".getBytes("UTF-8"))
      _ <- KbSync.writeBytes(upstream / "schemas" / "thing.yaml", "$id: thing\ntype: object\n".getBytes("UTF-8"))
    yield (kbRoot, bundleRoot, upstream)

  private def loadSync(kbRoot: Path): (Kb, SyncBundle) < (Sync & Abort[Throwable]) =
    for
      kb <- KbStore.load(kbRoot)
      sb <- KbSync.load(KbSync.findBundle(kb, None).get)
    yield (kb, sb.toOption.get)

  "pull" - {
    "imports concepts and assets, and records both in the lock" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb) <- loadSync(kbRoot)
        result <- KbSync.pull(sb, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb, result.lock)
        typesText <- (sb.localFile("docs/types.md")).read
        schemaText <- (sb.localFile("schemas/thing.yaml")).read
      yield
        assert(result.actions.count(_.verb == "added") == 3, s"got ${result.actions}")
        assert(typesText.contains("type: Specification Source"), "concepts gain a kb block")
        assert(typesText.contains("title: Types"), "upstream frontmatter is preserved")
        assert(schemaText == "$id: thing\ntype: object\n", "assets are byte-identical")
        assert(result.lock.files.count(_.kind == SyncKind.Asset) == 1, s"got ${result.lock.files}")
    }
    "writes nothing under --dry-run" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb) <- loadSync(kbRoot)
        result <- KbSync.pull(sb, upstream, "deadbeef", today, dryRun = true, theirs = false, prune = false)
        landed <- sb.localFile("docs/types.md").exists
      yield
        assert(result.actions.nonEmpty, "it still reports what it would do")
        assert(!landed, "dry run must not write")
    }
    "regression: a mirrored index.md is a concept, not a sub-index" in {
      // `kindOf` reserves index.md for the bundle. Inside a mirror the name is upstream's, and upstream puts
      // frontmatter in it — which used to be reported as `subindex-has-frontmatter`.
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb) <- loadSync(kbRoot)
        result <- KbSync.pull(sb, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb, result.lock)
        kb <- KbStore.load(kbRoot)
        b = kb.bundle("vendored").get
        mirrored = b.concepts.find(_.rel.mkString("/") == "sources/docs/index.md")
      yield
        assert(mirrored.isDefined, s"got ${b.concepts.map(_.rel.mkString("/"))}")
        assert(mirrored.get.kind == DocKind.Concept, s"got ${mirrored.get.kind}")
        assert(mirrored.get.vendored, "and it is marked as vendored")
    }
    "regression: refuses a manifest path that escapes the mirror" in {
      // The same hole `add-concept` had: a relative path with `..` writes outside the bundle it claims to be in.
      assert(!KbSync.safeRelative("../escaped.md"))
      assert(!KbSync.safeRelative("/etc/passwd"))
      assert(KbSync.safeRelative("docs/spec/types.md"))
    }
  }

  "status" - {
    "is clean straight after a pull, and local-only after an edit" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        (_, sb) <- loadSync(kbRoot)
        clean <- KbSync.status(sb, Some(upstream))
        before <- sb.localFile("docs/types.md").read
        _ <- sb.localFile("docs/types.md").write(before + "\nAn extra line.\n")
        (_, sb2) <- loadSync(kbRoot)
        edited <- KbSync.status(sb2, Some(upstream))
      yield
        assert(clean.forall(_.state == SyncState.Clean), s"got ${clean.map(r => r.path -> r.state.label)}")
        assert(
          edited.count(_.state == SyncState.LocalOnly) == 1,
          s"got ${edited.map(r => r.path -> r.state.label)}"
        )
    }
    "reports upstream-only when upstream moves and we did not" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        _ <- KbSync.writeBytes(upstream / "docs" / "types.md", "---\ntitle: Types\ndescription: Changed.\n---\n\n# Types\n".getBytes("UTF-8"))
        (_, sb) <- loadSync(kbRoot)
        rows <- KbSync.status(sb, Some(upstream))
      yield assert(
        rows.exists(r => r.path == "docs/types.md" && r.state == SyncState.UpstreamOnly),
        s"got ${rows.map(r => r.path -> r.state.label)}"
      )
    }
    "reports diverged when both sides moved" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        _ <- KbSync.writeBytes(upstream / "docs" / "types.md", "---\ntitle: Types\ndescription: Theirs.\n---\n\n# Types\n".getBytes("UTF-8"))
        before <- sb0.localFile("docs/types.md").read
        _ <- sb0.localFile("docs/types.md").write(before + "\nOurs.\n")
        (_, sb) <- loadSync(kbRoot)
        rows <- KbSync.status(sb, Some(upstream))
      yield assert(
        rows.exists(r => r.path == "docs/types.md" && r.state == SyncState.Diverged),
        s"got ${rows.map(r => r.path -> r.state.label)}"
      )
    }
  }

  "push" - {
    "writes back only what changed here, and in upstream's form" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        before <- sb0.localFile("docs/types.md").read
        _ <- sb0.localFile("docs/types.md").write(before + "\nA sentence we added.\n")
        (_, sb) <- loadSync(kbRoot)
        target <- scratch("export")
        out <- KbSync.push(sb, target, Some(upstream), dryRun = false, includeDiverged = false)
        (actions, refused) = out
        exported <- (target / "docs" / "types.md").read
        untouched <- (target / "schemas" / "thing.yaml").exists
      yield
        assert(refused.isEmpty, s"got $refused")
        assert(actions.map(_.path) == Seq("docs/types.md"), s"got ${actions.map(_.path)}")
        assert(!exported.contains("kb:begin"), "the kb block must never reach upstream")
        assert(!exported.contains("kb_upstream"), "nor its keys")
        assert(exported == "---\ntitle: Types\ndescription: The types.\n---\n\n# Types\n\nA sentence we added.\n", s"got: $exported")
        assert(!untouched, "unchanged files are not written")
    }
    "regression: holds back a diverged file rather than overwriting upstream's change" in {
      // push used to compare against the lockfile alone. A file that had moved on both sides then looked merely
      // local-only, and exporting it silently discarded upstream's edit.
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        _ <- KbSync.writeBytes(upstream / "docs" / "types.md", "---\ntitle: Types\ndescription: Theirs.\n---\n\n# Types\n".getBytes("UTF-8"))
        before <- sb0.localFile("docs/types.md").read
        _ <- sb0.localFile("docs/types.md").write(before + "\nOurs.\n")
        (_, sb) <- loadSync(kbRoot)
        target <- scratch("export-diverged")
        held <- KbSync.push(sb, target, Some(upstream), dryRun = false, includeDiverged = false)
        landed <- (target / "docs" / "types.md").exists
        forced <- KbSync.push(sb, target, Some(upstream), dryRun = false, includeDiverged = true)
        forcedLanded <- (target / "docs" / "types.md").exists
      yield
        assert(!landed, "a diverged file must not be exported by default")
        assert(held._1.exists(_.verb == "held back"), s"and it must be reported: ${held._1}")
        assert(held._2.nonEmpty, "it counts as refused, so the command exits non-zero")
        assert(forcedLanded, "--include-diverged is the explicit override")
        assert(forced._1.exists(_.verb == "wrote"), s"got ${forced._1}")
    }
  }

  "check findings" - {
    "flags a mirrored file the lockfile expects but the mirror lacks" in {
      for
        (kbRoot, _, upstream) <- syncFixture
        (_, sb0) <- loadSync(kbRoot)
        result <- KbSync.pull(sb0, upstream, "deadbeef", today, dryRun = false, theirs = false, prune = false)
        _ <- KbSync.writeLock(sb0, result.lock)
        _ <- KbSync.deleteFile(sb0.localFile("docs/types.md"))
        (kb, sb) <- loadSync(kbRoot)
        findings <- KbSync.checkFindings(kb, sb, None)
      yield assert(
        findings.exists(f => f.check == "sync-lock-drift" && f.severity == Severity.Error),
        s"got ${findings.map(_.check)}"
      )
    }
  }

// ------------------------------------------------------------------------- meta

/** Guards the one way this suite can lie: a spec that exists but is never run.
  *
  * kyo-test discovers suites through `META-INF/services/kyo.test.Test` and nowhere else — its runner accepts no suite
  * arguments. Forgetting an entry does not fail, it silently skips, and CI stays green over untested code. That is
  * the same shape of silent rot the knowledge base checks exist to prevent, so it gets a check rather than a note.
  */
class KbMetaSpec extends Test[Any]:

  "suite registry" - {
    "lists every spec defined in KbTests.scala" in {
      for
        root <- Sync.defer(Path(sys.env.getOrElse("MILL_WORKSPACE_ROOT", sys.props.getOrElse("user.dir", "."))))
        source <- (root / "KbTests.scala").read
        registry <- (root / "test-resources" / "META-INF" / "services" / "kyo.test.Test").read
      yield
        val defined = raw"class\s+(\w+)\s+extends\s+Test\[".r.findAllMatchIn(source).map(_.group(1)).toSet
        val registered =
          registry.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSet
        assert(defined.nonEmpty, "found no suites in KbTests.scala — has the file moved?")
        assert(
          defined == registered,
          s"registry and source disagree.\n  defined but never run: ${(defined -- registered).toSeq.sorted}" +
            s"\n  registered but absent: ${(registered -- defined).toSeq.sorted}"
        )
    }
  }

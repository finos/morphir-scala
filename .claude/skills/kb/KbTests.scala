//| scalaVersion: 3.8.4
//| mainClass: KbTests
//| moduleDeps: [KbIntentEdit.scala, KbRefresh.scala, KbRender.scala]

/** Test suite for the kb and intent skill code.
  *
  * Mill's single-file script mode exposes no test module — `mill test <script>` does not resolve — so this is a
  * plain executable with a small harness rather than munit or kyo-test. It runs the same way everything else here
  * does, and exits non-zero on failure so CI can gate on it.
  *
  * {{{
  * .claude/skills/kb/mill KbTests.scala
  * }}}
  *
  * Cases marked *regression* pin behaviour that was once wrong. Three of them came out of the Codex review on #936
  * and one out of dogfooding; none would have been caught by the manual testing that preceded them.
  */

import kyo.*
import java.time.LocalDate

object KbTests extends KyoApp:

  // ------------------------------------------------------------------ harness

  private val cases = collection.mutable.ArrayBuffer.empty[(String, Unit < (Async & Abort[Throwable]))]

  private def test(name: String)(body: => Unit < (Async & Abort[Throwable])): Unit =
    cases += (name -> body)

  private def expect(cond: Boolean, msg: => String): Unit < Abort[Throwable] =
    if cond then () else Abort.fail(AssertionError(msg))

  private def eq[A](actual: A, expected: A, label: String): Unit < Abort[Throwable] =
    expect(actual == expected, s"$label\n     expected: $expected\n     actual:   $actual")

  private def contains(haystack: String, needle: String, label: String): Unit < Abort[Throwable] =
    expect(haystack.contains(needle), s"$label\n     expected to contain: $needle\n     actual:\n$haystack")

  private val today = LocalDate.parse("2026-07-28")

  /** A fresh scratch directory per test. Nothing here touches the real knowledge base. */
  private def scratch(label: String): Path < (Sync & Abort[Throwable]) =
    Sync.defer {
      val dir = java.nio.file.Files.createTempDirectory(s"kb-test-$label-").toAbsolutePath.toString
      Path(dir)
    }

  /** A minimal knowledge base: one ordinary bundle, optionally an intent bundle. */
  private def fixture(withIntent: Boolean): Path < (Sync & Abort[Throwable]) =
    for
      root <- scratch("kb")
      kbRoot = root / "kb"
      _ <- KbScaffold.newBundle(kbRoot, "demo", None, "Demo", "A scratch bundle.", "0.2", today)
      _ <-
        if !withIntent then ((): Unit < (Sync & Abort[Throwable]))
        else
          KbIntentEdit
            .initBundle(kbRoot, "intent", Some("pkg:pypi/demo"), Some("demo"), 60, today)
            .map(_ => ())
    yield kbRoot

  // ------------------------------------------------------------------- parsing

  test("splitFrontmatter returns None when there is no fence") {
    val (fm, body) = KbStore.splitFrontmatter("# Title\n\nprose\n")
    eq(fm, None, "frontmatter").andThen(contains(body, "# Title", "body preserved"))
  }

  test("splitFrontmatter separates a normal block") {
    val (fm, body) = KbStore.splitFrontmatter("---\ntype: Concept\n---\n\n# Title\n")
    for
      _ <- eq(fm, Some("type: Concept\n"), "frontmatter")
      _ <- contains(body, "# Title", "body")
      _ <- expect(!body.contains("type:"), "body must not carry the frontmatter")
    yield ()
  }

  test("splitFrontmatter normalises CRLF") {
    val (fm, _) = KbStore.splitFrontmatter("---\r\ntype: Concept\r\n---\r\n\r\nbody\r\n")
    eq(fm, Some("type: Concept\n"), "frontmatter from CRLF input")
  }

  test("splitFrontmatter treats an unterminated fence as no frontmatter") {
    val (fm, body) = KbStore.splitFrontmatter("---\ntype: Concept\n\nnever closed\n")
    eq(fm, None, "frontmatter").andThen(contains(body, "never closed", "whole text returned as body"))
  }

  test("parseFrontmatter rejects duplicate keys") {
    KbStore.parseFrontmatter("type: A\ntype: B\n") match
      case Left(_) => ()
      case Right(_) => Abort.fail(AssertionError("duplicate keys should not parse"))
  }

  test("parseFrontmatter rejects a non-mapping document") {
    KbStore.parseFrontmatter("- just\n- a list\n") match
      case Left(msg) => contains(msg, "expected a mapping", "error message")
      case Right(_) => Abort.fail(AssertionError("a list is not frontmatter"))
  }

  test("regression: an unquoted date reads as a string, not as absent") {
    // SnakeYAML resolves `2026-07-28` to a java.util.Date. Without explicit handling every date-valued field —
    // OKF's stale_after, intent's created and state_since — silently read as None.
    KbStore.parseFrontmatter("type: Intent\ncreated: 2026-07-28\n") match
      case Left(e) => Abort.fail(AssertionError(s"should parse: $e"))
      case Right(fm) => eq(fm.str("created"), Some("2026-07-28"), "created")
  }

  test("frontmatter accessors read tags and nested sources") {
    KbStore.parseFrontmatter(
      "type: Concept\ntags: [a, b]\nsources:\n  - id: s1\n    resource: https://example.com/x.md\n    title: X\n"
    ) match
      case Left(e) => Abort.fail(AssertionError(s"should parse: $e"))
      case Right(fm) =>
        for
          _ <- eq(fm.tags, List("a", "b"), "tags")
          _ <- eq(fm.sources.size, 1, "source count")
          _ <- eq(fm.sources.head.id, Some("s1"), "source id")
          _ <- eq(fm.sources.head.resource, "https://example.com/x.md", "source resource")
        yield ()
  }

  test("extractLinks reports line numbers offset past the frontmatter") {
    val links = KbStore.extractLinks("intro\n\n[one](/a.md) and [two](https://x)\n", frontmatterLines = 5)
    for
      _ <- eq(links.map(_.dest), Seq("/a.md", "https://x"), "destinations")
      _ <- eq(links.head.line, 8, "line number includes the frontmatter offset")
      _ <- expect(links(1).isExternal, "https link is external")
      _ <- expect(links.head.isBundleRelative, "leading slash is bundle-relative")
    yield ()
  }

  test("extractLinks ignores links inside fenced code") {
    val links = KbStore.extractLinks("```\n[not a link](/nope.md)\n```\n\n[real](/yes.md)\n", 0)
    eq(links.map(_.dest), Seq("/yes.md"), "only the prose link")
  }

  test("headings skip fenced code blocks") {
    val hs = KbIndex.headings("# Real\n\n```bash\n# just a shell comment\n```\n\n## Also real\n")
    eq(hs.map(_._2), Seq("Real", "Also real"), "heading texts")
  }

  // --------------------------------------------------------------------- paths

  test("segmentsUnder relativises and refuses unrelated paths") {
    val base = Path("/a/b")
    for
      _ <- eq(KbPath.segmentsUnder(Path("/a/b/c/d.md"), base), Some(Seq("c", "d.md")), "under")
      _ <- eq(KbPath.segmentsUnder(Path("/x/y"), base), None, "not under")
      _ <- expect(KbPath.isUnder(Path("/a/b/c"), base), "isUnder")
    yield ()
  }

  test("DocRef parses bundle:path and rejects anything else") {
    for
      _ <- eq(DocRef.parse("morphir/morphir-scala:/x.md"), Some(DocRef("morphir/morphir-scala", "/x.md")), "valid")
      _ <- eq(DocRef.parse("no-colon"), None, "missing colon")
      _ <- eq(DocRef.parse("bundle:relative.md"), None, "path must be bundle-relative")
      _ <- eq(DocRef.parse("pkg:maven/org/x@1.0"), None, "a purl is not a DocRef")
    yield ()
  }

  test("intent enums parse and carry their tier") {
    for
      _ <- eq(IntentState.parse("in-progress"), Some(IntentState.InProgress), "hyphenated state")
      _ <- eq(IntentState.parse("nonsense"), None, "unknown state")
      _ <- expect(IntentState.Released.isTerminal, "Released is terminal")
      _ <- expect(!IntentState.Backlog.isActive, "Backlog is not active — a backlog is meant to sit")
      _ <- expect(IntentKind.parse("feature").exists(_.userVisible), "feature is user-visible")
      _ <- expect(IntentKind.parse("build").exists(!_.userVisible), "build is internal")
    yield ()
  }

  test("slugify produces kebab-case") {
    eq(KbScaffold.slugify("  Release Labels, v2! "), "release-labels-v2", "slug")
  }

  // ------------------------------------------------------------- frontmatter IO

  test("regression: setKeys appends after a trailing block, not inside it") {
    // Anchoring on "the last top-level line" put the new key between `sources:` and its children, corrupting both
    // the YAML and the provenance. Found by review, not by manual testing.
    for
      dir <- scratch("setkeys")
      f = dir / "doc.md"
      _ <- f.write("---\ntype: Intent\nstate: Backlog\nsources:\n  - id: s1\n    resource: https://x/y.md\n---\n\nbody\n")
      _ <- KbIntentEdit.setKeys(f, Seq("capability" -> Some("demo:/cap.md")))
      text <- f.read
      parsed = KbStore.parseFrontmatter(KbStore.splitFrontmatter(text)._1.getOrElse(""))
      _ <- parsed match
        case Left(e) => Abort.fail(AssertionError(s"frontmatter no longer parses: $e\n$text"))
        case Right(fm) =>
          for
            _ <- eq(fm.str("capability"), Some("demo:/cap.md"), "new key")
            _ <- eq(fm.sources.size, 1, "sources survived")
            _ <- eq(fm.sources.head.resource, "https://x/y.md", "source resource survived")
          yield ()
    yield ()
  }

  test("setKeys replaces an existing key in place and leaves the body alone") {
    for
      dir <- scratch("setkeys2")
      f = dir / "doc.md"
      _ <- f.write("---\ntype: Intent\nstate: Backlog\n---\n\n# Body\n\nprose\n")
      _ <- KbIntentEdit.setKeys(f, Seq("state" -> Some("Released")))
      text <- f.read
      _ <- contains(text, "state: Released", "state updated")
      _ <- expect(!text.contains("state: Backlog"), "old value gone")
      _ <- contains(text, "# Body", "body preserved")
    yield ()
  }

  // ------------------------------------------------------------------ scaffold

  test("regression: add-concept refuses paths that escape the bundle") {
    for
      kbRoot <- fixture(withIntent = false)
      kb <- KbStore.load(kbRoot)
      b = kb.bundle("demo").get
      res <- KbScaffold.addConcept(b, "../escaped.md", "Concept", "X", "Y.", Nil, None, Nil, "Orientation", None, today)
      _ <- res match
        case Left(err) => contains(err, "must stay inside the bundle", "refusal message")
        case Right(_) => Abort.fail(AssertionError("path traversal must be refused"))
      leaked <- (kbRoot / "bundles" / "escaped.md").exists
      _ <- expect(!leaked, "nothing may be written outside the bundle")
    yield ()
  }

  test("add-concept refuses the reserved OKF filenames") {
    for
      kbRoot <- fixture(withIntent = false)
      kb <- KbStore.load(kbRoot)
      b = kb.bundle("demo").get
      res <- KbScaffold.addConcept(b, "index.md", "Concept", "X", "Y.", Nil, None, Nil, "Orientation", None, today)
      _ <- res match
        case Left(err) => contains(err, "reserved", "refusal message")
        case Right(_) => Abort.fail(AssertionError("index.md must be refused"))
    yield ()
  }

  test("add-concept writes the concept and wires up index and log") {
    for
      kbRoot <- fixture(withIntent = false)
      kb <- KbStore.load(kbRoot)
      b = kb.bundle("demo").get
      _ <- KbScaffold.addConcept(b, "naming.md", "Concept", "Naming", "How things are named.", Seq("x"), Some("draft"), Nil, "Orientation", None, today)
      idx <- b.index.file.read
      log <- b.log.get.file.read
      concept <- (b.root / "naming.md").read
      _ <- contains(idx, "[Naming](/naming.md) - How things are named.", "index entry mirrors the description")
      _ <- contains(log, "**Creation**", "log entry")
      _ <- contains(concept, "type: Concept", "concept frontmatter")
      _ <- contains(concept, "status: draft", "status")
    yield ()
  }

  test("appendLogEntry creates a date section, then appends within it") {
    for
      dir <- scratch("log")
      f = dir / "log.md"
      _ <- f.write("# Log\n")
      _ <- KbScaffold.appendLogEntry(f, today, "**Creation**: one.")
      _ <- KbScaffold.appendLogEntry(f, today, "**Update**: two.")
      text <- f.read
      _ <- eq(text.linesIterator.count(_.startsWith(s"## $today")), 1, "one date heading, not two")
      _ <- contains(text, "* **Creation**: one.", "first entry")
      _ <- contains(text, "* **Update**: two.", "second entry")
    yield ()
  }

  // -------------------------------------------------------------------- intent

  test("intent bundle is discovered by its marker, not by its path") {
    for
      kbRoot <- fixture(withIntent = true)
      kb <- KbStore.load(kbRoot)
      b <- KbIntent.findBundle(kb) match
        case Some(x) => (x: Bundle < Abort[Throwable])
        case None => Abort.fail(AssertionError("intent bundle not found"))
      cfg = KbIntent.config(b)
      _ <- eq(cfg.system, Some("pkg:pypi/demo"), "system purl")
      _ <- eq(cfg.capabilityBundle, Some("demo"), "capability bundle")
      _ <- eq(cfg.staleAfterDays, 60, "threshold")
    yield ()
  }

  test("intent ids allocate sequentially") {
    for
      kbRoot <- fixture(withIntent = true)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      _ <- eq(KbIntent.nextId(b), "0001", "first id")
      _ <- KbIntentEdit.create(b, "First", "One.", IntentKind.Feature, false, None, Nil, today)
      kb2 <- KbStore.load(kbRoot)
      b2 = KbIntent.findBundle(kb2).get
      _ <- eq(KbIntent.nextId(b2), "0002", "second id")
      _ <- eq(KbIntent.find(b2, "1").map(_.id), Some("0001"), "find by bare number")
    yield ()
  }

  test("releasing a user-visible intent demands a resolvable capability") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "Feature work", "Ships something.", IntentKind.Feature, false, None, Nil, today)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      i = KbIntent.find(b, "0001").get

      noCap <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released), today)
      _ <- noCap match
        case Left(e) => contains(e, "needs --capability", "missing capability refused")
        case Right(_) => Abort.fail(AssertionError("release without a capability must be refused"))

      malformed <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released, capability = Some("nope")), today)
      _ <- malformed match
        case Left(e) => contains(e, "is not `bundle-label:/path.md`", "malformed ref refused")
        case Right(_) => Abort.fail(AssertionError("a malformed reference must be refused"))

      unresolved <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released, capability = Some("demo:/missing.md")), today)
      _ <- unresolved match
        case Left(e) => contains(e, "names no concept", "unresolvable ref refused")
        case Right(_) => Abort.fail(AssertionError("an unresolvable reference must be refused"))
    yield ()
  }

  test("releasing an internal-kind intent needs no capability") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "Build work", "Internal only.", IntentKind.Build, false, None, Nil, today)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      i = KbIntent.find(b, "0001").get
      res <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Released), today)
      _ <- (res match
        case Left(e) => Abort.fail(AssertionError(s"internal kinds may release without a capability: $e"))
        case Right(_) => ()
      ): Unit < Abort[Throwable]
    yield ()
  }

  test("cancel demands a reason and supersede a known successor") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "A thing", "Something.", IntentKind.Feature, false, None, Nil, today)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      i = KbIntent.find(b, "0001").get

      noReason <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Cancelled), today)
      _ <- noReason match
        case Left(e) => contains(e, "needs --reason", "cancel refusal")
        case Right(_) => Abort.fail(AssertionError("cancel without a reason must be refused"))

      badSucc <- KbIntentEdit.transition(kb, b, i, KbIntentEdit.Transition(IntentState.Superseded, supersededBy = Some("0099")), today)
      _ <- badSucc match
        case Left(e) => contains(e, "no intent", "unknown successor refused")
        case Right(_) => Abort.fail(AssertionError("an unknown successor must be refused"))
    yield ()
  }

  test("intent check reports unmet obligations and stale active work") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "Stuck", "Sat in refinement.", IntentKind.Feature, false, None, Nil, today)
      kb1 <- KbStore.load(kbRoot)
      b1 = KbIntent.findBundle(kb1).get
      i = KbIntent.find(b1, "0001").get
      _ <- KbIntentEdit.transition(kb1, b1, i, KbIntentEdit.Transition(IntentState.Refinement), today)
      // Hand-edit into an impossible state to prove the checker, not the transition guard.
      _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state" -> Some("Released")))
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      findings = KbIntent.check(kb, b, today)
      _ <- expect(
        findings.exists(f => f.check == "intent-released-no-capability" && f.severity == Severity.Error),
        s"expected a released-without-capability error, got ${findings.map(_.check)}"
      )

      // Staleness is measured from state_since, and only for active states.
      _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state" -> Some("Refinement"), "state_since" -> Some("2026-01-01")))
      kbS <- KbStore.load(kbRoot)
      bS = KbIntent.findBundle(kbS).get
      stale = KbIntent.check(kbS, bS, today)
      _ <- expect(
        stale.exists(f => f.check == "intent-stale" && f.severity == Severity.Warn),
        s"expected a staleness warning, got ${stale.map(_.check)}"
      )

      _ <- KbIntentEdit.setKeys(i.doc.file, Seq("state" -> Some("Backlog")))
      kbB <- KbStore.load(kbRoot)
      bB = KbIntent.findBundle(kbB).get
      backlog = KbIntent.check(kbB, bB, today)
      _ <- expect(!backlog.exists(_.check == "intent-stale"), "Backlog is never stale")
    yield ()
  }

  test("the generated intent index groups by state and keeps the preamble") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "Open thing", "Still pending.", IntentKind.Feature, false, None, Nil, today)
      kb <- KbStore.load(kbRoot)
      b = KbIntent.findBundle(kb).get
      _ <- KbIntentEdit.generateIndex(b, today)
      text <- b.index.file.read
      _ <- contains(text, KbIntent.Marker, "marker retained")
      _ <- contains(text, "## Backlog (1)", "grouped heading with a count")
      _ <- contains(text, "- Still pending.", "entry description is verbatim")
      // Flags belong in the link text, never after the description — otherwise kb check sees drift.
      _ <- expect(!text.contains("- Still pending. _("), "no flags appended after the description")
    yield ()
  }

  // --------------------------------------------------------------------- check

  test("kb check flags a broken link, and --allow-dangling downgrades it") {
    for
      kbRoot <- fixture(withIntent = false)
      kb0 <- KbStore.load(kbRoot)
      b0 = kb0.bundle("demo").get
      _ <- KbScaffold.addConcept(b0, "a.md", "Concept", "A", "First.", Nil, None, Nil, "Orientation", None, today)
      _ <- (b0.root / "a.md").read.map(t => (b0.root / "a.md").write(t + "\nSee [missing](/gone.md).\n"))
      kb <- KbStore.load(kbRoot)
      strict <- KbCheck.run(kb, None, today)
      _ <- expect(
        strict.exists(f => f.check == "link-broken" && f.severity == Severity.Error),
        s"expected a broken-link error, got ${strict.map(f => f.check -> f.severity)}"
      )
      lenient <- KbCheck.run(kb, None, today, allowDangling = true)
      _ <- expect(
        lenient.exists(f => f.check == "link-broken" && f.severity == Severity.Warn),
        "--allow-dangling should downgrade, not suppress"
      )
    yield ()
  }

  test("kb check flags a concept with no type and one nothing indexes") {
    for
      kbRoot <- fixture(withIntent = false)
      kb0 <- KbStore.load(kbRoot)
      b0 = kb0.bundle("demo").get
      _ <- (b0.root / "orphan.md").write("---\ntitle: Orphan\n---\n\nNo type, nobody links to it.\n")
      kb <- KbStore.load(kbRoot)
      findings <- KbCheck.run(kb, None, today)
      _ <- expect(findings.exists(_.check == "concept-missing-type"), "missing type is an error")
      _ <- expect(findings.exists(_.check == "concept-not-indexed"), "unindexed concept is a warning")
    yield ()
  }

  test("kb check flags an index bullet that has drifted from its description") {
    for
      kbRoot <- fixture(withIntent = false)
      kb0 <- KbStore.load(kbRoot)
      b0 = kb0.bundle("demo").get
      _ <- KbScaffold.addConcept(b0, "a.md", "Concept", "A", "The real description.", Nil, None, Nil, "Orientation", None, today)
      _ <- b0.index.file.read.map(t => b0.index.file.write(t.replace("The real description.", "stale text")))
      kb <- KbStore.load(kbRoot)
      findings <- KbCheck.run(kb, None, today)
      _ <- expect(findings.exists(_.check == "index-description-drift"), "drift is detected")
      // …and refresh repairs it.
      actions <- KbRefresh.refreshMarkdown(kb, addMissing = false, section = "Orientation", dryRun = false)
      _ <- expect(actions.exists(_.kind == RefreshKind.DescriptionFixed), "refresh reports the fix")
      kb2 <- KbStore.load(kbRoot)
      after <- KbCheck.run(kb2, None, today)
      _ <- expect(!after.exists(_.check == "index-description-drift"), "drift is gone after refresh")
    yield ()
  }

  test("refresh --dry-run reports without writing") {
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
      _ <- expect(actions.nonEmpty, "dry run still reports")
      _ <- eq(after, before, "dry run must not write")
    yield ()
  }

  // --------------------------------------------------------------------- index

  test("the SQLite index records docs, links and frontmatter facets") {
    for
      kbRoot <- fixture(withIntent = true)
      kb0 <- KbStore.load(kbRoot)
      b0 = KbIntent.findBundle(kb0).get
      _ <- KbIntentEdit.create(b0, "Indexed thing", "For the index.", IntentKind.Feature, true, None, Seq("x"), today)
      kb <- KbStore.load(kbRoot)
      db = kbRoot / "index.db"
      stats <- KbIndex.build(kb, db)
      _ <- expect(stats.docs > 0, "documents indexed")
      rows <- KbIndex.query(db, "SELECT state, kind, breaking FROM v_intent")
      _ <- rows match
        case Left(e) => Abort.fail(AssertionError(s"v_intent query failed: $e"))
        case Right(r) =>
          for
            _ <- eq(r.rows.size, 1, "one intent row")
            _ <- eq(r.rows.head.head, Some("Backlog"), "state pivoted from frontmatter")
            _ <- eq(r.rows.head(1), Some("feature"), "kind pivoted")
            _ <- eq(r.rows.head(2), Some("true"), "breaking pivoted")
          yield ()
    yield ()
  }

  test("query refuses anything that is not read-only") {
    for
      kbRoot <- fixture(withIntent = false)
      kb <- KbStore.load(kbRoot)
      db = kbRoot / "index.db"
      _ <- KbIndex.build(kb, db)
      res <- KbIndex.query(db, "DELETE FROM doc")
      _ <- (res match
        case Left(e) => contains(e, "read-only", "refusal message")
        case Right(_) => Abort.fail(AssertionError("a write must be refused"))
      ): Unit < Abort[Throwable]
    yield ()
  }

  test("regression: index status notices a document deleted since the build") {
    // Staleness compared modification times, and a deleted file has none — so the index kept serving a document
    // that no longer existed. Found by review.
    for
      kbRoot <- fixture(withIntent = false)
      kb0 <- KbStore.load(kbRoot)
      b0 = kb0.bundle("demo").get
      _ <- KbScaffold.addConcept(b0, "doomed.md", "Concept", "Doomed", "About to go.", Nil, None, Nil, "Orientation", None, today)
      kb <- KbStore.load(kbRoot)
      db = kbRoot / "index.db"
      _ <- KbIndex.build(kb, db)
      fresh <- KbIndex.status(db, kb)
      _ <- fresh match
        case Right((_, _, stale)) => eq(stale, Seq.empty[String], "index is fresh immediately after building")
        case Left(e) => Abort.fail(AssertionError(e))
      _ <- (b0.root / "doomed.md").remove
      kb2 <- KbStore.load(kbRoot)
      after <- KbIndex.status(db, kb2)
      _ <- after match
        case Right((_, _, stale)) =>
          expect(stale.exists(_.contains("removed since")), s"deletion should mark the index stale, got $stale")
        case Left(e) => Abort.fail(AssertionError(e))
    yield ()
  }

  // -------------------------------------------------------------------- runner

  run {
    for
      results <- Kyo.foreach(Chunk.from(cases.toSeq)) { (name, body) =>
        Abort.run[Throwable](body).map {
          case Result.Success(_) => (name, None)
          case r => (name, Some(Option(r.failure.map(_.toString).getOrElse("failed")).getOrElse("failed")))
        }
      }
      failures = results.filter(_._2.isDefined)
      _ <- Kyo.foreachDiscard(results) { (name, err) =>
        err match
          case None => Console.printLine(s"  ok    $name")
          case Some(msg) => Console.printLine(s"  FAIL  $name\n        $msg")
      }
      _ <- Console.printLine(s"\n${results.size - failures.size}/${results.size} passed")
      _ <-
        if failures.isEmpty then ((): Unit < (Async & Abort[Throwable]))
        else Sync.defer(java.lang.System.exit(1))
    yield ()
  }

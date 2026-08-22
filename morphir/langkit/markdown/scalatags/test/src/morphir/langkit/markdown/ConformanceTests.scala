package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.Parser

/**
 * Measures the parse-and-compile path against a vendored conformance suite, profile by profile.
 *
 * This is a **ratchet, not a pass/fail gate**. A profile we do not yet implement in full would otherwise leave only two
 * bad options: delete the fixtures that fail, or pretend the parser does more than it does. Instead the score is
 * recorded and defended — a drop fails the build, and a rise asks for the recorded score to be raised.
 *
 * What is recorded is a *ratio against a named version*, not a bare number. `652` on its own says nothing: it is a
 * different claim against CommonMark 0.31.2, which has 652 examples, than against a suite with more. So each profile
 * records its version, the fixture file it was measured from, and both halves of the ratio — and the total is checked
 * against the file, so re-vendoring a different version fails loudly rather than quietly re-basing the claim.
 *
 * The comparison is byte-exact with no canonicalization, which is the whole reason ScalaTags is the oracle rather than
 * kyo-ui. See
 * [[https://github.com/finos/morphir-scala/blob/main/kb/bundles/intent/0033-markdown-compilation.md intent 0033]].
 *
 * Runs on JVM, JS and Native. The fixtures are read as ordinary files through [[kyo.Path]], whose JS backend is
 * `node:fs` and whose Native backend is the javalib's `java.nio`; the directory holding them arrives from the build
 * (see `MorphirMarkdownConformanceEnv` and `MorphirMarkdownConformanceJsEnv` in `testing.mill`), because neither Mill's
 * test sandbox nor Node's working directory is dependable. An earlier version read them through the classloader, which
 * is what confined it to the JVM.
 */
class ConformanceTests extends Test[Any]:

  private val BaselineFile = "conformance-baselines.json"

  /**
   * Absolute directory holding the fixture files, supplied by the build.
   *
   * Produced by the `markdownConformanceFixtures` task in `testing.mill`: the CommonMark fixture and the conformance
   * baselines are vendored and copied through unchanged, and the GFM fixture is derived from GitHub's specification
   * text, since GitHub publishes no JSON form of its own. `MorphirMarkdownConformanceEnv` (JVM and Native) and
   * `MorphirMarkdownConformanceJsEnv` (JS) — both also in `testing.mill` — pass that task's output directory to this
   * test through `MORPHIR_CONFORMANCE_FIXTURES`.
   *
   * Resolved through [[kyo.Flag]] for the same reason [[reportFailuresIn]] is: it is the one env-and-property reader
   * that works the same on all three platforms, reading Node's `process.env` under Scala.js rather than
   * `System.getenv`, which always returns null there. The property form is for anyone running this suite outside Mill.
   */
  private val fixturesDir: String =
    val configured = Flag[String]("morphir.conformance.fixtures", "")
    if configured.nonEmpty then configured
    else
      throw new IllegalStateException(
        "morphir.conformance.fixtures is unset, so the harness cannot find its fixtures. Mill sets it from " +
          "MorphirMarkdownConformanceEnv (JVM and Native) and MorphirMarkdownConformanceJsEnv (JS); outside Mill, " +
          "pass the absolute path to a directory holding commonmark-0.31.2-spec.json, gfm-0.29-spec.json and " +
          "conformance-baselines.json — the same three files markdownConformanceFixtures produces."
      )

  /**
   * One example a profile records rather than targets, with why.
   *
   * A conformance suite can be wrong about its own dialect. Deleting the example would hide that; counting it as a
   * failure forever says we have work left that we do not. Recording it keeps the example, subtracts it from the
   * measured set, and makes the reason travel with the score.
   */
  private final case class Divergence(example: Int, reason: String) derives Schema

  /**
   * One profile's recorded conformance.
   *
   * `total` is not derived from the fixture file on purpose. Holding it here is what turns "we pass 652" into "we pass
   * 652 of the 652 in CommonMark 0.31.2", and what makes vendoring a different suite a deliberate act rather than an
   * accident that silently moves the goalposts.
   */
  private final case class Baseline(
      profile: String,
      version: String,
      fixtures: String,
      passing: Int,
      total: Int,
      divergences: Chunk[Divergence] = Chunk.empty
  ) derives Schema:
    def name: String = s"$profile $version"

    /** Example numbers this profile does not target. */
    def divergedExamples: Set[Int] = divergences.map(_.example).toSet

  /**
   * The part of a fixture entry this harness reads. The published JSON also carries `start_line` and `end_line`, which
   * decoding ignores.
   */
  private final case class Example(markdown: String, html: String, example: Int, section: String) derives Schema

  /**
   * Reads one fixture file eagerly, outside the effect system.
   *
   * `Path#read` is `Sync & Abort[FileReadException]`, but the baselines are needed while the test tree is being built —
   * `baselines.foreach` below declares one pair of tests per profile — and a test tree cannot be constructed inside a
   * `Sync`. The unsafe view is the honest way to say that: this is a test harness reading a file it vendors itself, at
   * class-initialization time, and a failure should abort the suite rather than be threaded anywhere.
   */
  private def readFixture(name: String): String =
    import AllowUnsafe.embrace.danger
    val file = (Path(fixturesDir) / name).unsafe
    file.read() match
      case Result.Success(text) => text
      case other => throw new IllegalStateException(s"missing or unreadable fixture ${file.show}: $other")

  private def decode[A](fixture: String)(using Schema[A], reflect.ClassTag[A]): A =
    Json.decode[A](readFixture(fixture)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $fixture: $other")

  private lazy val baselines: Chunk[Baseline] = decode[Chunk[Baseline]](BaselineFile)

  /**
   * The parse profile a recorded profile is measured under.
   *
   * A conformance suite measures a dialect, and a dialect here is an [[MdProfile]] — so the mapping has to be explicit
   * rather than defaulted, otherwise a GFM suite would be scored against a CommonMark parse and the extensions would
   * read as unimplemented forever. An unrecognised name fails loudly for the same reason: a typo in the baselines file
   * must not silently fall back to the base grammar.
   */
  private def profileOf(baseline: Baseline): MdProfile = baseline.profile match
    case "CommonMark"               => MdProfile.commonmark
    case "GitHub Flavored Markdown" => MdProfile.gfm
    case other                      =>
      throw new IllegalStateException(
        s"$BaselineFile names profile '$other', which this harness cannot map to an MdProfile. " +
          "Add it to profileOf alongside the profile's fixtures."
      )

  private def examplesOf(baseline: Baseline): Chunk[Example] = decode[Chunk[Example]](baseline.fixtures)

  /**
   * How much of this profile's measured set another loaded profile also carries, and how much of it passes.
   *
   * Computed by comparing source-and-expected-HTML pairs rather than stored, because the duplication is a property of
   * the published suites and not something we should have to maintain a second copy of. A dialect that is a superset of
   * another shares most of its examples — GFM carries 636 CommonMark entries verbatim — and a score that does not
   * separate the two reads as if every example were about the extension under construction.
   */
  private def sharedWith(baseline: Baseline)(using MdProfile): Chunk[(String, Int, Int)] =
    val measured = examplesOf(baseline).filterNot(example => baseline.divergedExamples.contains(example.example))
    Chunk.from(
      baselines.filterNot(_.profile == baseline.profile).map { other =>
        val theirs = examplesOf(other).map(example => (example.markdown, example.html)).toSet
        val shared = measured.filter(example => theirs.contains((example.markdown, example.html)))
        (other.name, shared.size, shared.count(conforms))
      }
    )

  /** True when our parse-and-compile reproduces the fixture's expected HTML byte for byte. */
  private def conforms(example: Example)(using MdProfile): Boolean =
    Parser.parse(example.markdown) match
      case Result.Success(document) => ScalatagsCompiler.render(document) == example.html
      case _                        => false

  /**
   * Sections whose failures should be printed in full.
   *
   * A score says how far off we are; it does not say what to write next. This does: name `all`, or a comma-separated
   * list of section-name fragments, and every failure in those sections is printed with its source, what we produced
   * and what the spec wants. That is how a slice gets picked, and it is off by default because the output is long and
   * only someone driving the score wants it.
   *
   * Read through [[kyo.Flag]] rather than off the environment directly, which is what the rest of a Kyo codebase does.
   * The flag resolves `morphir.conformance.failures` as a system property first and then as the environment variable of
   * the same name, dots turned into underscores and the letters uppercased.
   *
   * {{{
   * MORPHIR_CONFORMANCE_FAILURES='block quotes,lists' ./mill -i morphir.langkit.markdown.scalatags.jvm.test
   * }}}
   *
   * The environment variable is the route that works through Mill, and `-i` is required: the daemon carries no variable
   * it was not started with, and a `-D` on the Mill command line sets a property on the launcher rather than on the
   * forked test JVM. The property route is there for anyone running this suite outside Mill.
   */
  private val reportFailuresIn: Chunk[String] =
    Chunk.from(
      Flag[String]("morphir.conformance.failures", "")
        .split(",")
        .map(_.trim.toLowerCase)
        .filter(_.nonEmpty)
    )

  private def selectedForReport(example: Example): Boolean =
    reportFailuresIn.exists(selector => selector == "all" || example.section.toLowerCase.contains(selector))

  /** Newlines shown as `\n`, so one example stays on one line and a missing trailing newline is visible. */
  private def oneLine(text: String): String = text.replace("\n", "\\n")

  private def reportFailure(example: Example)(using MdProfile) =
    val produced = Parser.parse(example.markdown) match
      case Result.Success(document) => oneLine(ScalatagsCompiler.render(document))
      case other                    => s"<parse failed: $other>"
    Console.printLine(
      s"  [${example.section}] example ${example.example}\n" +
        s"    source   ${oneLine(example.markdown)}\n" +
        s"    produced $produced\n" +
        s"    expected ${oneLine(example.html)}"
    )

  "conformance" - {

    "records at least one profile" in
      assert(baselines.nonEmpty, s"$BaselineFile lists no profiles, so nothing is being measured")

    "every recorded divergence names an example the fixture holds" in {
      baselines.foreach { baseline =>
        val numbers = examplesOf(baseline).map(_.example).toSet
        baseline.divergences.foreach { divergence =>
          assert(
            numbers.contains(divergence.example),
            s"${baseline.name} records a divergence for example ${divergence.example}, which ${baseline.fixtures} " +
              "does not hold. A divergence has to point at a real example or it is unfalsifiable."
          )
          assert(
            divergence.reason.trim.nonEmpty,
            s"${baseline.name} records a divergence for example ${divergence.example} with no reason. " +
              "A divergence without a reason is indistinguishable from a bug we stopped counting."
          )
        }
      }
      succeed("an empty divergence list has nothing to check, and should not read as unevaluated")
    }

    baselines.foreach { baseline =>
      given MdProfile = profileOf(baseline)

      s"${baseline.name} vendors the ${baseline.total + baseline.divergences.size} examples it claims, numbered with no gaps" in {
        val examples = examplesOf(baseline)
        val vendored = baseline.total + baseline.divergences.size
        assert(
          examples.size == vendored,
          s"${baseline.fixtures} holds ${examples.size} examples and $BaselineFile claims $vendored " +
            s"(${baseline.total} measured plus ${baseline.divergences.size} recorded as divergent). " +
            "Vendoring a different version means updating the profile's version, total and passing count together."
        )
        // Guards the vendored file against truncation or reordering: our own unit tests cite examples by number, so a
        // shifted numbering would silently repoint every one of those citations.
        assert(examples.map(_.example).toSeq == (1 to vendored).toSeq)
      }

      s"${baseline.name} measures the suite minus its recorded divergences" in {
        val examples = examplesOf(baseline)
        val measured = examples.filterNot(example => baseline.divergedExamples.contains(example.example))
        assert(
          measured.size == baseline.total,
          s"${baseline.name} claims a total of ${baseline.total} and measures ${measured.size} " +
            s"(${examples.size} in the fixture, ${baseline.divergences.size} recorded as divergent). " +
            "The recorded total is the size of the measured set, not the size of the file."
        )
      }

      s"${baseline.name} does not fall below ${baseline.passing}/${baseline.total}" in {
        val examples = examplesOf(baseline).filterNot(example => baseline.divergedExamples.contains(example.example))
        val passing  = examples.count(conforms)
        val total    = examples.size

        val bySection = examples
          .groupBy(_.section)
          .map((section, group) => (section, group.count(conforms), group.size))
          .toSeq
          .sortBy((section, passed, size) => (-passed, section))

        val untouched = bySection.count((_, passed, _) => passed == 0)
        val reported  = examples.filter(example => selectedForReport(example) && !conforms(example))

        for
          _ <- Console.printLine(
            f"${baseline.name}: $passing/$total (${passing * 100.0 / total}%.1f%%), recorded ${baseline.passing}/${baseline.total}"
          )
          _ <- Kyo.foreachDiscard(bySection.filter((_, passed, _) => passed > 0)) { (section, passed, size) =>
            Console.printLine(f"    $passed%3d/$size%-3d  $section")
          }
          _ <-
            if untouched > 0 then Console.printLine(s"    (and $untouched sections with nothing passing yet)")
            else Kyo.unit
          _ <-
            if reported.nonEmpty then
              Console.printLine(s"failing examples in the selected sections (${reported.size}):")
                .andThen(Kyo.foreachDiscard(reported)(reportFailure))
            else Kyo.unit
          _ <-
            if baseline.divergences.isEmpty then Kyo.unit
            else
              Console.printLine(s"    ${baseline.divergences.size} divergence(s) recorded, not measured:")
                .andThen(Kyo.foreachDiscard(baseline.divergences) { divergence =>
                  Console.printLine(f"      example ${divergence.example}%4d  ${divergence.reason}")
                })
          _ <- Kyo.foreachDiscard(sharedWith(baseline)) { (other, shared, passed) =>
            Console.printLine(f"    shared with $other: $passed/$shared; specific to ${baseline.profile}: " +
              f"${passing - passed}/${total - shared}")
          }
          _ = assert(
            passing >= baseline.passing,
            s"${baseline.name} conformance regressed: $passing passing, ${baseline.passing} recorded. " +
              "Something stopped rendering the way the fixtures expect."
          )
          _ <-
            if passing > baseline.passing then
              Console.printLine(
                s">>> ${baseline.name} rose to $passing/$total. " +
                  s"Raise its passing count in $BaselineFile from ${baseline.passing} to $passing."
              )
            else Kyo.unit
        yield ()
      }
    }
  }

package morphir.langkit.markdown

import kyo.*
import kyo.test.*

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
 * JVM-only because it reads files. The compiler it measures is cross-platform and tested on all three.
 */
class ConformanceTests extends Test[Any]:

  private val BaselineResource = "conformance-baselines.json"

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
      total: Int
  ) derives Schema:
    def name: String = s"$profile $version"

  /**
   * The part of a fixture entry this harness reads. The published JSON also carries `start_line` and `end_line`, which
   * decoding ignores.
   */
  private final case class Example(markdown: String, html: String, example: Int, section: String) derives Schema

  private def readResource(name: String): String =
    val stream = Option(getClass.getClassLoader.getResourceAsStream(name))
      .getOrElse(throw new IllegalStateException(s"missing test resource: $name"))
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()

  private def decode[A](resource: String)(using Schema[A], reflect.ClassTag[A]): A =
    Json.decode[A](readResource(resource)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $resource: $other")

  private lazy val baselines: Chunk[Baseline] = decode[Chunk[Baseline]](BaselineResource)

  private def examplesOf(baseline: Baseline): Chunk[Example] = decode[Chunk[Example]](baseline.fixtures)

  /** True when our parse-and-compile reproduces the fixture's expected HTML byte for byte. */
  private def conforms(example: Example): Boolean =
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

  private def reportFailure(example: Example): Unit =
    println(s"  [${example.section}] example ${example.example}")
    println(s"    source   ${oneLine(example.markdown)}")
    Parser.parse(example.markdown) match
      case Result.Success(document) => println(s"    produced ${oneLine(ScalatagsCompiler.render(document))}")
      case other                    => println(s"    produced <parse failed: $other>")
    println(s"    expected ${oneLine(example.html)}")

  "conformance" - {

    "records at least one profile" in
      assert(baselines.nonEmpty, s"$BaselineResource lists no profiles, so nothing is being measured")

    baselines.foreach { baseline =>
      s"${baseline.name} vendors the ${baseline.total} examples it claims, numbered with no gaps" in {
        val examples = examplesOf(baseline)
        assert(
          examples.size == baseline.total,
          s"${baseline.fixtures} holds ${examples.size} examples and $BaselineResource claims ${baseline.total}. " +
            "Vendoring a different version means updating the profile's version, total and passing count together."
        )
        // Guards the vendored file against truncation or reordering: our own unit tests cite examples by number, so a
        // shifted numbering would silently repoint every one of those citations.
        assert(examples.map(_.example).toSeq == (1 to baseline.total).toSeq)
      }

      s"${baseline.name} does not fall below ${baseline.passing}/${baseline.total}" in {
        val examples = examplesOf(baseline)
        val passing  = examples.count(conforms)
        val total    = examples.size

        val bySection = examples
          .groupBy(_.section)
          .map((section, group) => (section, group.count(conforms), group.size))
          .toSeq
          .sortBy((section, passed, size) => (-passed, section))

        println(
          f"${baseline.name}: $passing/$total (${passing * 100.0 / total}%.1f%%), recorded ${baseline.passing}/${baseline.total}"
        )
        bySection.filter((_, passed, _) => passed > 0).foreach { (section, passed, size) =>
          println(f"    $passed%3d/$size%-3d  $section")
        }
        val untouched = bySection.count((_, passed, _) => passed == 0)
        if untouched > 0 then println(s"    (and $untouched sections with nothing passing yet)")

        val reported = examples.filter(example => selectedForReport(example) && !conforms(example))
        if reported.nonEmpty then
          println(s"failing examples in the selected sections (${reported.size}):")
          reported.foreach(reportFailure)

        assert(
          passing >= baseline.passing,
          s"${baseline.name} conformance regressed: $passing passing, ${baseline.passing} recorded. " +
            "Something stopped rendering the way the fixtures expect."
        )
        if passing > baseline.passing then
          println(
            s">>> ${baseline.name} rose to $passing/$total. " +
              s"Raise its passing count in $BaselineResource from ${baseline.passing} to $passing."
          )
      }
    }
  }

package morphir.langkit.markdown

import kyo.*
import kyo.test.*

/**
 * Measures the parse-and-compile path against the vendored CommonMark 0.31.2 fixtures.
 *
 * This is a **ratchet, not a pass/fail gate**. The AST reaches five block kinds and carries no inline nodes, so most of
 * the 652 examples cannot pass until intent 0021 widens it. Gating on total conformance would leave only two bad
 * options: delete the fixtures that fail, or pretend the parser does more than it does. Instead the score is recorded
 * and defended — a drop fails the build, and a rise asks for the baseline to be raised.
 *
 * The comparison is byte-exact with no canonicalization, which is the whole reason ScalaTags is the oracle rather than
 * kyo-ui. See
 * [[https://github.com/finos/morphir-scala/blob/main/kb/bundles/intent/0033-markdown-compilation.md intent 0033]].
 *
 * JVM-only because it reads a file. The compiler it measures is cross-platform and tested on all three.
 */
class CommonMarkConformanceTests extends Test[Any]:

  private val SpecResource     = "commonmark-0.31.2-spec.json"
  private val BaselineResource = "conformance-baseline.txt"

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

  private lazy val examples: Chunk[Example] =
    Json.decode[Chunk[Example]](readResource(SpecResource)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $SpecResource: $other")

  /** True when our parse-and-compile reproduces the fixture's expected HTML byte for byte. */
  private def conforms(example: Example): Boolean =
    Parser.parse(example.markdown) match
      case Result.Success(document) => ScalatagsCompiler.render(document) == example.html
      case _                        => false

  "CommonMark 0.31.2 conformance" - {

    "vendors the whole example set, numbered 1 to 652 with no gaps" in {
      assert(examples.size == 652)
      // Guards the vendored file against truncation or reordering: our own unit tests cite examples by number, so a
      // shifted numbering would silently repoint every one of those citations.
      assert(examples.map(_.example).toSeq == (1 to 652).toSeq)
    }

    "does not fall below the recorded baseline" in {
      val passing  = examples.count(conforms)
      val baseline = readResource(BaselineResource).trim.toInt
      val total    = examples.size

      val bySection = examples
        .groupBy(_.section)
        .map((section, group) => (section, group.count(conforms), group.size))
        .toSeq
        .sortBy((section, passed, size) => (-passed, section))

      println(f"CommonMark 0.31.2: $passing/$total (${passing * 100.0 / total}%.1f%%), baseline $baseline")
      bySection.filter((_, passed, _) => passed > 0).foreach { (section, passed, size) =>
        println(f"    $passed%3d/$size%-3d  $section")
      }
      val untouched = bySection.count((_, passed, _) => passed == 0)
      println(s"    (and $untouched sections with nothing passing yet)")

      assert(
        passing >= baseline,
        s"conformance regressed: $passing passing, baseline $baseline. " +
          "A block kind stopped rendering the way the fixtures expect."
      )
      if passing > baseline then
        println(s">>> Conformance rose to $passing. Raise $BaselineResource from $baseline to $passing.")
    }
  }

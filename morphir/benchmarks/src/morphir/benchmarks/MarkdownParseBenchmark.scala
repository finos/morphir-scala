package morphir.benchmarks

import java.util.concurrent.TimeUnit
import morphir.langkit.markdown.{Parser, ScalatagsCompiler}
import org.openjdk.jmh.annotations.*

/**
 * Parser benchmarks, shaped to answer one question: does a change to the parser's loops cost anything?
 *
 * The set is deliberately not "run the conformance suite and time it". That corpus is 652 mostly two-line examples, so
 * an aggregate over it is dominated by per-call overhead rather than by the loops under change, and a bad regression in
 * one construct disappears into the average. It is kept as one macro benchmark, for the mix, and the work is done by
 * three other kinds:
 *
 *   - **per construct**, so a change to a specific loop shows up against the loop it changed;
 *   - **at three sizes**, because the real risk in this parser is not a constant factor but an accidental quadratic:
 *     `closingRun`, `labelEnd` and the emphasis opener search all scan inside an outer scan;
 *   - **adversarial**, because [[morphir.langkit.core.scanner.ScanBudget]] exists to bound hostile input, and the
 *     shapes it defends against are exactly the ones with the worst asymptotics.
 *
 * Read `parseScaling*` together, not individually. Linear growth in the ratio is the property being defended; a single
 * input's time in isolation cannot show it.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class MarkdownParseBenchmark:

  import MarkdownParseBenchmark.*

  // ---- per construct: one loop each, so a regression names itself ----

  @Benchmark def parseParagraphs(): Unit     = parse(Paragraphs)
  @Benchmark def parseHeadings(): Unit       = parse(Headings)
  @Benchmark def parseEmphasis(): Unit       = parse(Emphasis)
  @Benchmark def parseCodeSpans(): Unit      = parse(CodeSpans)
  @Benchmark def parseLinks(): Unit          = parse(Links)
  @Benchmark def parseReferenceLinks(): Unit = parse(ReferenceLinks)
  @Benchmark def parseFencedCode(): Unit     = parse(FencedCode)
  @Benchmark def parseIndentedCode(): Unit   = parse(IndentedCode)
  @Benchmark def parseLists(): Unit          = parse(Lists)
  @Benchmark def parseHtmlBlocks(): Unit     = parse(HtmlBlocks)

  // ---- scaling: the ratios matter, not the absolutes ----

  @Benchmark def parseScalingSmall(): Unit  = parse(MixedSmall)
  @Benchmark def parseScalingMedium(): Unit = parse(MixedMedium)
  @Benchmark def parseScalingLarge(): Unit  = parse(MixedLarge)

  // ---- adversarial: the shapes the scan budget exists for ----

  @Benchmark def parseUnmatchedBackticks(): Unit = parse(UnmatchedBackticks)
  @Benchmark def parseDelimiterRuns(): Unit      = parse(DelimiterRuns)
  @Benchmark def parseNestedBrackets(): Unit     = parse(NestedBrackets)
  @Benchmark def parseUnclosedLinks(): Unit      = parse(UnclosedLinks)

  // ---- realistic: what the knowledge browser actually renders ----

  @Benchmark def parseRealisticDocument(): Unit = parse(Realistic)

  /** Both halves of the path a reader sees, so a parser win is not paid for at the writer. */
  @Benchmark def parseAndRenderRealistic(): Unit =
    Parser.parse(Realistic) match
      case kyo.Result.Success(document) => consume(ScalatagsCompiler.render(document))
      case other                        => throw new IllegalStateException(s"benchmark input failed to parse: $other")

  private def parse(source: String): Unit =
    Parser.parse(source) match
      case kyo.Result.Success(document) => consume(document.blocks.size)
      case other                        => throw new IllegalStateException(s"benchmark input failed to parse: $other")

  /** Keep the result reachable so the JIT cannot delete the work. */
  private def consume(value: Any): Unit = sink = value

  private var sink: Any = null

end MarkdownParseBenchmark

object MarkdownParseBenchmark:

  private def repeat(unit: String, times: Int): String = Seq.fill(times)(unit).mkString("\n")

  val Paragraphs: String   = repeat("The quick brown fox jumps over the lazy dog, and then does it again.\n", 200)
  val Headings: String     = repeat("# A heading\n\n## A deeper heading\n", 200)
  val Emphasis: String     = repeat("Some *emphasised* and **strong** and ***both*** text in a line.\n", 200)
  val CodeSpans: String    = repeat("Call `parse` then ``render `this` `` and finally `compile`.\n", 200)
  val Links: String        = repeat("See [the docs](https://example.com/a/b \"Title\") and ![img](/i.png).\n", 200)
  val FencedCode: String   = repeat("```scala\nval x = 1\nval y = 2\n```\n", 200)
  val IndentedCode: String = repeat("    val x = 1\n    val y = 2\n\ntext\n", 200)
  val Lists: String        = repeat("- one\n- two\n- three\n\n1. first\n2. second\n", 200)
  val HtmlBlocks: String   = repeat("<div class=\"note\">\ncontent\n</div>\n\ntext\n", 200)

  /** Definitions at the end, so every reference resolves forward and exercises the two-phase parse. */
  val ReferenceLinks: String =
    repeat("Text with [a reference][one] and [another][two] in it.\n", 200) +
      "\n[one]: https://example.com/one \"One\"\n[two]: https://example.com/two \"Two\"\n"

  private val MixedUnit =
    """# Heading
      |
      |A paragraph with *emphasis*, `code`, and [a link](https://example.com "T").
      |
      |- a list item
      |- another with **strong** text
      |
      |```scala
      |val x = 1
      |```
      |""".stripMargin

  val MixedSmall: String  = repeat(MixedUnit, 1)
  val MixedMedium: String = repeat(MixedUnit, 10)
  val MixedLarge: String  = repeat(MixedUnit, 100)

  /** No run ever closes, so every one rescans to end of line before giving up. */
  val UnmatchedBackticks: String = repeat("`" * 60 + " text that never closes the span\n", 100)

  /** Long runs that can both open and close, which is the worst case for the opener search and the rule of three. */
  val DelimiterRuns: String = repeat("*" * 40 + "text" + "*" * 40 + "\n", 100)

  /** Deeply nested labels, so `labelEnd` walks the whole line for each opener. */
  val NestedBrackets: String = repeat("[" * 30 + "text" + "]" * 30 + "(/url)\n", 100)

  /** Openers with no target, so each one is attempted and abandoned. */
  val UnclosedLinks: String = repeat("[a](unclosed [b](also unclosed [c](\n", 100)

  /** A document shaped like a knowledge-base concept: prose first, with structure through it. */
  val Realistic: String = repeat(
    """## Section
      |
      |Prose explaining something, with a [reference](https://example.com) and `an inline call`,
      |running across two lines so soft breaks are exercised.
      |
      |- a point
      |- a second point with *emphasis*
      |
      |> Not yet a block quote to this parser, but present in real documents.
      |
      |```scala
      |def example(x: Int): Int = x + 1
      |```
      |
      |A closing paragraph.
      |""".stripMargin,
    20
  )
end MarkdownParseBenchmark

package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.cst.{Cst, CstParser, Lower}

/**
 * The same conformance measurement as [[ConformanceTests]], through the other pipeline: parse to the CST, lower to the
 * AST, render. Equality with the direct parse is not the claim — inline text segmentation may differ node for node —
 * rendering byte-for-byte against the spec is, over every example. This is what makes `Lower.lower` trustworthy as
 * *the* producer of the AST: the CST carries everything the document means, and lowering loses none of it.
 */
class LoweredConformanceTests extends Test[Any]:

  private val SpecFixture = "commonmark-0.31.2-spec.json"

  private final case class Example(markdown: String, html: String, example: Int, section: String) derives Schema

  private def readFixture(name: String): String =
    import AllowUnsafe.embrace.danger
    val dir = Flag[String]("morphir.conformance.fixtures", "")
    if dir.isEmpty then
      throw new IllegalStateException(
        "morphir.conformance.fixtures is unset; Mill sets it through the MorphirMarkdownConformance* traits."
      )
    val file = (Path(dir) / name).unsafe
    file.read() match
      case Result.Success(text) => text
      case other => throw new IllegalStateException(s"missing or unreadable fixture ${file.show}: $other")

  private lazy val examples: Chunk[Example] =
    Json.decode[Chunk[Example]](readFixture(SpecFixture)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $SpecFixture: $other")

  private def rendered(source: String): String =
    ScalatagsCompiler.render(Lower.lower(CstParser.parse(source)))

  private def oneLine(text: String): String = text.replace("\n", "\\n")

  "lowered conformance" - {

    s"CommonMark 0.31.2 renders 652/652 through parse-to-CST then lower" in {
      assert(examples.size == 652)
      val failures = examples.filterNot(example => rendered(example.markdown) == example.html)
      for
        _ <- Kyo.foreachDiscard(failures.take(10)) { example =>
          Console.printLine(
            s"  [${example.section}] example ${example.example}\n" +
              s"    source   ${oneLine(example.markdown)}\n" +
              s"    lowered  ${oneLine(rendered(example.markdown))}\n" +
              s"    expected ${oneLine(example.html)}"
          )
        }
        _ <-
          if failures.size > 10 then Console.printLine(s"  ... and ${failures.size - 10} more failures")
          else Kyo.unit
      yield assert(failures.isEmpty, s"${failures.size} of ${examples.size} examples fail through the lowered pipeline")
    }
  }

package morphir.langkit.markdown

import kyo.*
import kyo.test.*

/**
 * The rewrite's forcing invariant, from the first slice on: `Cst.print(CstParser.parse(source))` reproduces the source
 * byte for byte, over the whole vendored CommonMark corpus, and every parsed tree tiles — its leaves partition the
 * document with no gap and no overlap.
 *
 * Green from day one because an unstructured region is a [[MdCstNode.Verbatim]] leaf, which prints as itself. Later
 * slices graduate constructs out of verbatim into typed nodes; this suite is what proves each graduation lost nothing.
 * See morphir-lc8.20 and intent 0021.
 */
class CstRoundTripTests extends Test[Any]:

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

  /** Newlines shown as `\n` so a mismatch prints on one line and a missing trailing newline is visible. */
  private def oneLine(text: String): String = text.replace("\n", "\\n")

  private def roundTrips(source: String, label: String)(using AssertScope): Unit =
    val document = CstParser.parse(source)
    val printed  = Cst.print(document)
    assert(
      printed == source,
      s"$label did not round-trip.\n  source  ${oneLine(source)}\n  printed ${oneLine(printed)}"
    )
    val errors = Cst.tilingErrors(document, source.length)
    assert(errors.isEmpty, s"$label violates tiling: ${errors.mkString("; ")}")

  "round-trip" - {

    "the empty document" in roundTrips("", "empty document")

    "a document with no trailing newline" in roundTrips("plain text", "no trailing newline")

    "CRLF line endings survive" in roundTrips("alpha\r\nbeta\r\n", "CRLF document")

    "a lone carriage return survives" in roundTrips("alpha\rbeta", "lone CR document")

    s"all examples in $SpecFixture, byte-exact, with tiling intact" in {
      assert(examples.size == 652)
      examples.foreach(example => roundTrips(example.markdown, s"example ${example.example} [${example.section}]"))
    }
  }

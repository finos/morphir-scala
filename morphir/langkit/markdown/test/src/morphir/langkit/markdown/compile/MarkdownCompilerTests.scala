package morphir.langkit.markdown.compile

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

class MarkdownCompilerTests extends Test[Any]:

  /**
   * A debug writer that renders an S-expression rather than markup.
   *
   * Deliberately not HTML: it proves the driver visits every node and nests children correctly, without tying the
   * traversal test to any output target.
   */
  private given Compiler[String] with
    def document(children: Chunk[String]): String    = children.mkString("(doc ", " ", ")")
    def heading(level: HeadingLevel, text: String)   = s"(h${level.toInt} $text)"
    def paragraph(text: String): String              = s"(p $text)"
    def fencedCode(info: FenceInfo, content: String) = s"(code:${info.language.getOrElse("-")} $content)"
    def unorderedList(items: Chunk[String]): String  = items.mkString("(ul ", " ", ")")
    def listItem(text: String): String               = s"(li $text)"
    def thematicBreak: String                        = "(hr)"
  end given

  private val span = Span.zero

  private def compile(blocks: Block*): String =
    MarkdownCompiler.compile[String](Document(Chunk.from(blocks), span))

  "MarkdownCompiler.compile" - {
    "folds an empty document" in
      assert(compile() == "(doc )")
    "folds each block kind in document order" in {
      val actual = compile(
        Block.Heading(HeadingLevel.One, "Title", span),
        Block.Paragraph("Body", span),
        Block.ThematicBreak(span)
      )
      assert(actual == "(doc (h1 Title) (p Body) (hr))")
    }
    "compiles list items before the list that holds them" in
      assert(compile(Block.UnorderedList(Chunk("one", "two"), span)) == "(doc (ul (li one) (li two)))")
    "passes the fence info through to the code node" in
      assert(compile(Block.FencedCode(FenceInfo.parse("scala"), "x", span)) == "(doc (code:scala x))")
    "carries the heading level rather than flattening it" in
      assert(compile(Block.Heading(HeadingLevel.Six, "Deep", span)) == "(doc (h6 Deep))")
  }

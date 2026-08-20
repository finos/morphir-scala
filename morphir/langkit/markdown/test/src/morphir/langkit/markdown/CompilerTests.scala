package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

class CompilerTests extends Test[Any]:

  /**
   * A debug writer that renders an S-expression rather than markup.
   *
   * Deliberately not HTML: it proves the fold visits every node and nests children correctly, without tying the
   * traversal test to any output target.
   */
  private given Compiler[String] with
    def document(children: Chunk[String]): String             = children.mkString("(doc ", " ", ")")
    def heading(level: HeadingLevel, children: Chunk[String]) = s"(h${level.toInt} ${children.mkString(" ")})"
    def paragraph(children: Chunk[String]): String            = s"(p ${children.mkString(" ")})"
    def fencedCode(info: FenceInfo, content: String)          = s"(code:${info.language.getOrElse("-")} $content)"
    def unorderedList(items: Chunk[String]): String           = items.mkString("(ul ", " ", ")")
    def listItem(children: Chunk[String]): String             = s"(li ${children.mkString(" ")})"
    def text(value: String): String                           = value
    def codeSpan(value: String): String                       = s"(code-span $value)"
    def emphasis(children: Chunk[String]): String             = s"(em ${children.mkString(" ")})"
    def strongEmphasis(children: Chunk[String]): String       = s"(strong ${children.mkString(" ")})"
    def link(destination: String, title: Maybe[String], children: Chunk[String]) =
      s"(link $destination ${title.getOrElse("-")} ${children.mkString(" ")})"
    def image(destination: String, title: Maybe[String], alt: String) =
      s"(img $destination ${title.getOrElse("-")} $alt)"
    def thematicBreak: String = "(hr)"
  end given

  private val span = Span.zero

  private def prose(value: String): Chunk[Inline] = Chunk(Inline.Text(value, span))
  private def item(value: String): ListItem       = ListItem(prose(value), span)

  private def compile(blocks: Block*): String =
    Compiler.compile[String](Document(Chunk.from(blocks), span))

  "Compiler.compile" - {
    "folds an empty document" in
      assert(compile() == "(doc )")
    "folds each block kind in document order" in {
      val actual = compile(
        Block.Heading(HeadingLevel.One, prose("Title"), span),
        Block.Paragraph(prose("Body"), span),
        Block.ThematicBreak(span)
      )
      assert(actual == "(doc (h1 Title) (p Body) (hr))")
    }
    "compiles list items before the list that holds them" in
      assert(compile(Block.UnorderedList(Chunk(item("one"), item("two")), span)) == "(doc (ul (li one) (li two)))")
    "passes the fence info through to the code node" in
      assert(compile(Block.FencedCode(FenceInfo.parse("scala"), "x", span)) == "(doc (code:scala x))")
    "carries the heading level rather than flattening it" in
      assert(compile(Block.Heading(HeadingLevel.Six, prose("Deep"), span)) == "(doc (h6 Deep))")
  }

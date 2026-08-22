package morphir.langkit.markdown

import kyo.*
import kyo.test.*

class CompilerTests extends Test[Any]:

  /**
   * A debug writer that renders an S-expression rather than markup.
   *
   * Deliberately not HTML: it proves the fold visits every node and nests children correctly, without tying the
   * traversal test to any output target.
   */
  private given Compiler[String] with
    def root(children: Chunk[String]): String                 = children.mkString("(doc ", " ", ")")
    def heading(depth: HeadingLevel, children: Chunk[String]) = s"(h${depth.toInt} ${children.mkString(" ")})"
    def paragraph(children: Chunk[String]): String            = s"(p ${children.mkString(" ")})"
    def code(info: FenceInfo, value: String): String          = s"(code:${info.language.getOrElse("-")} $value)"
    def list(ordered: Boolean, start: Maybe[ListStart], children: Chunk[String]): String =
      if ordered then children.mkString(s"(ol:${start.getOrElse(ListStart.One).toInt} ", " ", ")")
      else children.mkString("(ul ", " ", ")")
    def listItem(checked: Maybe[Boolean], children: Chunk[String]): String = s"(li ${children.mkString(" ")})"
    def text(value: String): String                                        = value
    def inlineCode(value: String): String                                  = s"(code-span $value)"
    def emphasis(children: Chunk[String]): String                          = s"(em ${children.mkString(" ")})"
    def strong(children: Chunk[String]): String                            = s"(strong ${children.mkString(" ")})"
    def delete(children: Chunk[String]): String                            = s"(del ${children.mkString(" ")})"
    def link(url: String, title: Maybe[String], children: Chunk[String])   =
      s"(link $url ${title.getOrElse("-")} ${children.mkString(" ")})"
    def image(url: String, title: Maybe[String], alt: String) =
      s"(img $url ${title.getOrElse("-")} $alt)"
    def inlineHtml(value: String): String           = s"(rawhtml $value)"
    def break: String                               = "(br)"
    def html(value: String): String                 = s"(html $value)"
    def blockquote(children: Chunk[String]): String = children.mkString("(quote ", " ", ")")
    def blockSeparator: String                      = "\\n"
    def thematicBreak: String                       = "(hr)"
  end given

  private def prose(value: String): Chunk[MdNode.PhrasingContent] = Chunk(MdNode.Text(value))
  private def item(value: String): MdNode.ListItem = MdNode.ListItem(Chunk(MdNode.Paragraph(prose(value))))

  private def compile(blocks: MdNode.FlowContent*): String =
    Compiler.compile[String](MdNode.Root(Chunk.from(blocks)))

  "Compiler.compile" - {
    "folds an empty document" in
      assert(compile() == "(doc )")
    "folds each block kind in document order" in {
      val actual = compile(
        MdNode.Heading(HeadingLevel.One, prose("Title")),
        MdNode.Paragraph(prose("Body")),
        MdNode.ThematicBreak()
      )
      assert(actual == "(doc (h1 Title) (p Body) (hr))")
    }
    "compiles list items before the list that holds them" in
      assert(compile(MdNode.List(ordered = false, start = Absent, spread = false, Chunk(item("one"), item("two")))) ==
        "(doc (ul (li one) (li two)))")
    "passes the fence info through to the code node" in
      assert(compile(MdNode.Code(FenceInfo.parse("scala"), "x")) == "(doc (code:scala x))")
    "carries the heading level rather than flattening it" in
      assert(compile(MdNode.Heading(HeadingLevel.Six, prose("Deep"))) == "(doc (h6 Deep))")
  }

package morphir.langkit.markdown

import kyo.*
import morphir.langkit.markdown.MdNode.*

/**
 * The authoring DSL for [[MdNode]]: scalatags-style call sites, varargs, string literals as text.
 *
 * Every combinator returns its precise case type, so the content-category unions ([[MdNode.FlowContent]],
 * [[MdNode.PhrasingContent]]) typecheck at the call site the way they typecheck on [[MdNode]] itself — a paragraph
 * accepts only phrasing content, never a heading. Every node constructs with [[MdMeta.empty]]: these are generated
 * nodes, not parsed ones, and their `span` is honestly `Absent`.
 *
 * `List` here means [[MdNode.List]], not `scala.collection.immutable.List` — the wildcard import shadows it, on
 * purpose, so `ul`/`ol` can return the node's own name.
 *
 * An object rather than a package, so [[MD]] can re-export it: Scala 3 exports members of a term, and a package is not
 * one. `import morphir.langkit.markdown.dsl.*` reads the same either way, so call sites are unaffected.
 */
object dsl:

  /** A bare string literal is authoring shorthand for a generated [[MdNode.Text]] node. */
  given Conversion[String, Text] = Text(_)

  def doc(children: FlowContent*): Root = Root(Chunk.from(children))

  /**
   * A document opening with frontmatter, curried so the body reads as a trailing varargs call:
   * `doc(frontmatter = yaml("..."))(p("hi"))`.
   */
  def doc(frontmatter: FrontMatter)(children: FlowContent*): Root = Root(Chunk.from(children), Present(frontmatter))

  /** A `---`-delimited YAML frontmatter block, authored from its raw document text. */
  def yaml(value: String): FrontMatter.Yaml = FrontMatter.Yaml(YamlDocText(value))

  def h1(children: PhrasingContent*): Heading = Heading(HeadingLevel.One, Chunk.from(children))
  def h2(children: PhrasingContent*): Heading = Heading(HeadingLevel.Two, Chunk.from(children))
  def h3(children: PhrasingContent*): Heading = Heading(HeadingLevel.Three, Chunk.from(children))
  def h4(children: PhrasingContent*): Heading = Heading(HeadingLevel.Four, Chunk.from(children))
  def h5(children: PhrasingContent*): Heading = Heading(HeadingLevel.Five, Chunk.from(children))
  def h6(children: PhrasingContent*): Heading = Heading(HeadingLevel.Six, Chunk.from(children))

  def p(children: PhrasingContent*): Paragraph = Paragraph(Chunk.from(children))

  /** Code with no info string. */
  def codeBlock(value: String): Code = Code(FenceInfo.empty, value)

  /** Fenced code with an info string, structurally parsed via [[FenceInfo.parse]]. */
  def codeBlock(info: String, value: String): Code = Code(FenceInfo.parse(info), value)

  def htmlBlock(value: String): Html = Html(value)

  def quote(children: FlowContent*): Blockquote = Blockquote(Chunk.from(children))

  def ul(items: ListItem*): List = List(ordered = false, start = Absent, spread = false, Chunk.from(items))
  def ul(spread: Boolean)(items: ListItem*): List =
    List(ordered = false, start = Absent, spread = spread, Chunk.from(items))
  def ol(items: ListItem*): List =
    List(ordered = true, start = Present(ListStart.One), spread = false, Chunk.from(items))

  /**
   * An ordered list counting from `start`. Write the number as a literal — `ol(ListStart(3))` — and the bound is
   * checked where it is written; use [[ListStart.fromInt]] when the number is computed.
   */
  def ol(start: ListStart, spread: Boolean = false)(items: ListItem*): List =
    List(ordered = true, start = Present(start), spread = spread, Chunk.from(items))

  /**
   * A list item from a mix of flow and phrasing content.
   *
   * One mixed-varargs signature, not two overloads: `ListItem*` and `PhrasingContent*` overloads erase identically.
   * Consecutive phrasing arguments accumulate into a single generated [[MdNode.Paragraph]]; each flow argument flushes
   * that accumulator first, so `li("a", "b", p("c"))` yields two items — `Paragraph(Text("a"), Text("b"))` then the
   * paragraph passed through unchanged — never an empty paragraph when the accumulator is empty at flush time.
   */
  def li(children: (FlowContent | PhrasingContent)*): ListItem =
    val flow                  = Chunk.newBuilder[FlowContent]
    val phrasing              = Chunk.newBuilder[PhrasingContent]
    def flushPhrasing(): Unit =
      if phrasing.knownSize > 0 then flow.addOne(Paragraph(phrasing.result()))
    children.foreach {
      case c: (Paragraph | Heading | Code | Html | Blockquote | List | ThematicBreak) =>
        flushPhrasing()
        flow.addOne(c)
      case c: (Text | InlineCode | Link | Image | Emphasis | Strong | InlineHtml | Break | Delete) =>
        phrasing.addOne(c)
    }
    flushPhrasing()
    ListItem(flow.result())

  def hr: ThematicBreak = ThematicBreak()

  def text(value: String): Text = Text(value)

  def code(value: String): InlineCode = InlineCode(value)

  def a(url: String)(children: PhrasingContent*): Link                = Link(url, Absent, Chunk.from(children))
  def a(url: String, title: String)(children: PhrasingContent*): Link =
    Link(url, Present(title), Chunk.from(children))

  def img(url: String, alt: String): Image                = Image(url, Absent, alt)
  def img(url: String, alt: String, title: String): Image = Image(url, Present(title), alt)

  def em(children: PhrasingContent*): Emphasis   = Emphasis(Chunk.from(children))
  def strong(children: PhrasingContent*): Strong = Strong(Chunk.from(children))

  /** GFM strikethrough, authored directly rather than through a parse. */
  def del(children: PhrasingContent*): Delete = Delete(Chunk.from(children))

  def inlineHtml(value: String): InlineHtml = InlineHtml(value)

  def br: Break = Break()
end dsl

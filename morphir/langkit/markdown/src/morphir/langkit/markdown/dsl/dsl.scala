package morphir.langkit.markdown.dsl

import kyo.*
import morphir.langkit.markdown.*
import morphir.langkit.markdown.MdcNode.*

/**
 * The authoring DSL for [[MdcNode]]: scalatags-style call sites, varargs, string literals as text.
 *
 * Every combinator returns its precise case type, so the content-category unions ([[FlowContent]], [[PhrasingContent]])
 * typecheck at the call site the way they typecheck on [[MdcNode]] itself — a paragraph accepts only phrasing content,
 * never a heading. Every node constructs with [[MdcMeta.empty]]: these are generated nodes, not parsed ones, and their
 * `span` is honestly `Absent`.
 *
 * `List` here means [[MdcNode.List]], not `scala.collection.immutable.List` — the wildcard import shadows it, on
 * purpose, so `ul`/`ol` can return the node's own name.
 */

/** A bare string literal is authoring shorthand for a generated [[Text]] node. */
given Conversion[String, Text] = Text(_)

def doc(children: FlowContent*): Root = Root(Chunk.from(children))

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
def ol(items: ListItem*): List = List(ordered = true, start = Present(1), spread = false, Chunk.from(items))
def ol(start: Int, spread: Boolean = false)(items: ListItem*): List =
  List(ordered = true, start = Present(start), spread = spread, Chunk.from(items))

/**
 * A list item from a mix of flow and phrasing content.
 *
 * One mixed-varargs signature, not two overloads: `ListItem*` and `PhrasingContent*` overloads erase identically.
 * Consecutive phrasing arguments accumulate into a single generated [[Paragraph]]; each flow argument flushes that
 * accumulator first, so `li("a", "b", p("c"))` yields two items — `Paragraph(Text("a"), Text("b"))` then the paragraph
 * passed through unchanged — never an empty paragraph when the accumulator is empty at flush time.
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
    case c: (Text | InlineCode | Link | Image | Emphasis | Strong | InlineHtml | Break) =>
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

def inlineHtml(value: String): InlineHtml = InlineHtml(value)

def br: Break = Break()

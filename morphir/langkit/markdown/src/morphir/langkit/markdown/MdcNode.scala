package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * The Markdown AST: one node type, mdast vocabulary, typed containment.
 *
 * The CST records what was written; this tree is what it means, produced only by lowering the CST. One sealed trait
 * gives uniform traversal — every node answers [[meta]], [[childNodes]] and [[literal]] — while the content-category
 * unions in the companion keep invalid states unrepresentable the way mdast's own content models do: a paragraph holds
 * phrasing content, never a heading. Cases are final case classes rather than enum cases because an enum case's
 * constructor widens to the enum type, which no union member ever is; a case class constructs at its precise type,
 * which is what lets `Paragraph(Chunk(Text("hi")))` compile. Source-form distinctions (fenced vs indented code, bullet
 * char, list delimiter) live in the CST alone.
 *
 * `Mdc` anticipates Markdown with frontmatter and components: [[MdcNode.FrontMatter]] is that seat, a field on
 * [[MdcNode.Root]] rather than a [[MdcNode.FlowContent]] member — frontmatter is only legal at document start, so
 * mid-document frontmatter is unrepresentable rather than a documented writer limit.
 */
sealed trait MdcNode derives CanEqual:

  /** What this node knows about itself beyond its content: its position, and whatever data a consumer attached. */
  def meta: MdcMeta

  /**
   * Where this node sits in the source, including its own delimiters; Absent marks a generated node.
   *
   * Derived from [[meta]] rather than stored, so the position stays one field of one record while every reader keeps
   * asking a node for its span directly.
   */
  def span: Maybe[Span] = meta.span

  /**
   * Every child in document order; empty for leaves. Total, for generic walks.
   *
   * Named `childNodes` (matching `MdcCstNode.childNodes`) rather than `children`: the cases' `children` fields carry
   * narrower category types, and a field may implement a trait member only at the exact same type.
   */
  def childNodes: Chunk[MdcNode] = this match
    case MdcNode.Root(children, frontmatter, _) => frontmatter.toChunk ++ children
    case MdcNode.Paragraph(children, _)         => children
    case MdcNode.Heading(_, children, _)        => children
    case MdcNode.Blockquote(children, _)        => children
    case MdcNode.List(_, _, _, children, _)     => children
    case MdcNode.ListItem(children, _)          => children
    case MdcNode.Link(_, _, children, _)        => children
    case MdcNode.Emphasis(children, _)          => children
    case MdcNode.Strong(children, _)            => children
    case _: (MdcNode.Code | MdcNode.Html | MdcNode.ThematicBreak | MdcNode.Text | MdcNode.InlineCode |
          MdcNode.Image | MdcNode.InlineHtml | MdcNode.Break | MdcNode.FrontMatter.Yaml) =>
      Chunk.empty

  /** The literal's text — code content, raw HTML, plain text; Absent for parents and markers. */
  def literal: Maybe[String] = this match
    case MdcNode.Text(value, _)             => Present(value)
    case MdcNode.InlineCode(value, _)       => Present(value)
    case MdcNode.Code(_, value, _)          => Present(value)
    case MdcNode.Html(value, _)             => Present(value)
    case MdcNode.InlineHtml(value, _)       => Present(value)
    case MdcNode.FrontMatter.Yaml(value, _) => Present(value.unwrap)
    case _                                  => Absent

  /**
   * This tree with every span stripped and every node's data kept, for structural comparison of generated against
   * parsed trees.
   *
   * Position is derived provenance — it says where the parser found the node — so a comparison that does not care where
   * the text sat drops it. Data is content the author attached, so it survives.
   */
  def unpositioned: MdcNode = this match
    case MdcNode.Root(children, frontmatter, meta) =>
      MdcNode.Root(
        children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]),
        frontmatter.map(_.unpositioned.asInstanceOf[MdcNode.FrontMatter]),
        meta.copy(span = Absent)
      )
    case MdcNode.Paragraph(children, meta) =>
      MdcNode.Paragraph(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), meta.copy(span = Absent))
    case MdcNode.Heading(depth, children, meta) =>
      MdcNode.Heading(
        depth,
        children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]),
        meta.copy(span = Absent)
      )
    case MdcNode.Code(info, value, meta)    => MdcNode.Code(info, value, meta.copy(span = Absent))
    case MdcNode.Html(value, meta)          => MdcNode.Html(value, meta.copy(span = Absent))
    case MdcNode.Blockquote(children, meta) =>
      MdcNode.Blockquote(children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]), meta.copy(span = Absent))
    case MdcNode.List(ordered, start, spread, children, meta) =>
      MdcNode.List(
        ordered,
        start,
        spread,
        children.map(item => item.unpositioned.asInstanceOf[MdcNode.ListItem]),
        meta.copy(span = Absent)
      )
    case MdcNode.ListItem(children, meta) =>
      MdcNode.ListItem(children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]), meta.copy(span = Absent))
    case MdcNode.ThematicBreak(meta)              => MdcNode.ThematicBreak(meta.copy(span = Absent))
    case MdcNode.Text(value, meta)                => MdcNode.Text(value, meta.copy(span = Absent))
    case MdcNode.InlineCode(value, meta)          => MdcNode.InlineCode(value, meta.copy(span = Absent))
    case MdcNode.Link(url, title, children, meta) =>
      MdcNode.Link(
        url,
        title,
        children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]),
        meta.copy(span = Absent)
      )
    case MdcNode.Image(url, title, alt, meta) => MdcNode.Image(url, title, alt, meta.copy(span = Absent))
    case MdcNode.Emphasis(children, meta)     =>
      MdcNode.Emphasis(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), meta.copy(span = Absent))
    case MdcNode.Strong(children, meta) =>
      MdcNode.Strong(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), meta.copy(span = Absent))
    case MdcNode.InlineHtml(value, meta)       => MdcNode.InlineHtml(value, meta.copy(span = Absent))
    case MdcNode.Break(meta)                   => MdcNode.Break(meta.copy(span = Absent))
    case MdcNode.FrontMatter.Yaml(value, meta) => MdcNode.FrontMatter.Yaml(value, meta.copy(span = Absent))

object MdcNode:

  /** What a block position may hold — mdast's flow content. */
  type FlowContent = Paragraph | Heading | Code | Html | Blockquote | List | ThematicBreak

  /** What a prose position may hold — mdast's phrasing content. */
  type PhrasingContent = Text | InlineCode | Link | Image | Emphasis | Strong | InlineHtml | Break

  /**
   * Front matter: metadata block a profile recognizes at document start. Cases nest here; Toml/Json are later members.
   */
  sealed trait FrontMatter extends MdcNode

  object FrontMatter:
    /** A `---`-delimited YAML block. `value` is the raw document text; decoding belongs to the consumer. */
    final case class Yaml(value: YamlDocText, meta: MdcMeta = MdcMeta.empty) extends FrontMatter

  // flow
  final case class Root(
      children: Chunk[FlowContent],
      frontmatter: Maybe[FrontMatter] = Absent,
      meta: MdcMeta = MdcMeta.empty
  ) extends MdcNode
  final case class Paragraph(children: Chunk[PhrasingContent], meta: MdcMeta = MdcMeta.empty) extends MdcNode
  final case class Heading(depth: HeadingLevel, children: Chunk[PhrasingContent], meta: MdcMeta = MdcMeta.empty)
      extends MdcNode

  /** Fenced and indented code both: the fence-or-indent distinction is the CST's. Indented code has empty info. */
  final case class Code(info: FenceInfo, value: String, meta: MdcMeta = MdcMeta.empty) extends MdcNode

  /** Block-position raw HTML. Its inline twin is [[InlineHtml]]; both project as Unist `html`. */
  final case class Html(value: String, meta: MdcMeta = MdcMeta.empty) extends MdcNode

  final case class Blockquote(children: Chunk[FlowContent], meta: MdcMeta = MdcMeta.empty) extends MdcNode

  /** `start` is Present only when `ordered`. `spread` is mdast's field; the renderer reads the derived `tight`. */
  final case class List(
      ordered: Boolean,
      start: Maybe[Int],
      spread: Boolean,
      children: Chunk[ListItem],
      meta: MdcMeta = MdcMeta.empty
  ) extends MdcNode

  final case class ListItem(children: Chunk[FlowContent], meta: MdcMeta = MdcMeta.empty) extends MdcNode
  final case class ThematicBreak(meta: MdcMeta = MdcMeta.empty)                          extends MdcNode

  // phrasing
  final case class Text(value: String, meta: MdcMeta = MdcMeta.empty)       extends MdcNode
  final case class InlineCode(value: String, meta: MdcMeta = MdcMeta.empty) extends MdcNode

  /** A link. An autolink lowers here too, its raw URI as both destination and only text child. */
  final case class Link(
      url: String,
      title: Maybe[String],
      children: Chunk[PhrasingContent],
      meta: MdcMeta = MdcMeta.empty
  ) extends MdcNode
  final case class Image(url: String, title: Maybe[String], alt: String, meta: MdcMeta = MdcMeta.empty) extends MdcNode
  final case class Emphasis(children: Chunk[PhrasingContent], meta: MdcMeta = MdcMeta.empty)            extends MdcNode
  final case class Strong(children: Chunk[PhrasingContent], meta: MdcMeta = MdcMeta.empty)              extends MdcNode
  final case class InlineHtml(value: String, meta: MdcMeta = MdcMeta.empty)                             extends MdcNode
  final case class Break(meta: MdcMeta = MdcMeta.empty)                                                 extends MdcNode

  extension (list: List)
    /** The renderer's positive: a tight list drops the `p` from its items. Derived, never stored. */
    def tight: Boolean = !list.spread

  extension [N <: MdcNode](node: N)
    /**
     * This node with one more typed annotation on it, at its own type.
     *
     * The result type is the node's own case, not [[MdcNode]], which is what lets an annotated node stay in the
     * content-category position it came from — a `Text` annotated is still phrasing content. Each arm rebuilds its own
     * case, so the cast back to `N` restores exactly the type the match tore off.
     */
    def withMeta[A](key: MetaKey[A], value: A): N =
      val updated         = node.meta.updated(key, value)
      val result: MdcNode = node match
        case n: Root             => n.copy(meta = updated)
        case n: Paragraph        => n.copy(meta = updated)
        case n: Heading          => n.copy(meta = updated)
        case n: Code             => n.copy(meta = updated)
        case n: Html             => n.copy(meta = updated)
        case n: Blockquote       => n.copy(meta = updated)
        case n: List             => n.copy(meta = updated)
        case n: ListItem         => n.copy(meta = updated)
        case n: ThematicBreak    => n.copy(meta = updated)
        case n: Text             => n.copy(meta = updated)
        case n: InlineCode       => n.copy(meta = updated)
        case n: Link             => n.copy(meta = updated)
        case n: Image            => n.copy(meta = updated)
        case n: Emphasis         => n.copy(meta = updated)
        case n: Strong           => n.copy(meta = updated)
        case n: InlineHtml       => n.copy(meta = updated)
        case n: Break            => n.copy(meta = updated)
        case n: FrontMatter.Yaml => n.copy(meta = updated)
      result.asInstanceOf[N]

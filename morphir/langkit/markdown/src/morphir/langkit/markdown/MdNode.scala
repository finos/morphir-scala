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
 * Frontmatter has a seat of its own: [[MdNode.FrontMatter]] is a field on [[MdNode.Root]] rather than a
 * [[MdNode.FlowContent]] member — frontmatter is only legal at document start, so mid-document frontmatter is
 * unrepresentable rather than a documented writer limit.
 */
sealed trait MdNode derives CanEqual:

  /** What this node knows about itself beyond its content: its position, and whatever data a consumer attached. */
  def meta: MdMeta

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
   * Named `childNodes` (matching `MdCstNode.childNodes`) rather than `children`: the cases' `children` fields carry
   * narrower category types, and a field may implement a trait member only at the exact same type.
   */
  def childNodes: Chunk[MdNode] = this match
    case MdNode.Root(children, frontmatter, _) => frontmatter.toChunk ++ children
    case MdNode.Paragraph(children, _)         => children
    case MdNode.Heading(_, children, _)        => children
    case MdNode.Blockquote(children, _)        => children
    case MdNode.List(_, _, _, children, _)     => children
    case MdNode.ListItem(children, _)          => children
    case MdNode.Link(_, _, children, _)        => children
    case MdNode.Emphasis(children, _)          => children
    case MdNode.Strong(children, _)            => children
    case _: (MdNode.Code | MdNode.Html | MdNode.ThematicBreak | MdNode.Text | MdNode.InlineCode |
          MdNode.Image | MdNode.InlineHtml | MdNode.Break | MdNode.FrontMatter.Yaml) =>
      Chunk.empty

  /** The literal's text — code content, raw HTML, plain text; Absent for parents and markers. */
  def literal: Maybe[String] = this match
    case MdNode.Text(value, _)             => Present(value)
    case MdNode.InlineCode(value, _)       => Present(value)
    case MdNode.Code(_, value, _)          => Present(value)
    case MdNode.Html(value, _)             => Present(value)
    case MdNode.InlineHtml(value, _)       => Present(value)
    case MdNode.FrontMatter.Yaml(value, _) => Present(value.unwrap)
    case _                                 => Absent

  /**
   * This tree with every span stripped and every node's data kept, for structural comparison of generated against
   * parsed trees.
   *
   * Position is derived provenance — it says where the parser found the node — so a comparison that does not care where
   * the text sat drops it. Data is content the author attached, so it survives.
   */
  def unpositioned: MdNode = this match
    case MdNode.Root(children, frontmatter, meta) =>
      MdNode.Root(
        children.map(_.unpositioned.asInstanceOf[MdNode.FlowContent]),
        frontmatter.map(_.unpositioned.asInstanceOf[MdNode.FrontMatter]),
        meta.copy(span = Absent)
      )
    case MdNode.Paragraph(children, meta) =>
      MdNode.Paragraph(children.map(_.unpositioned.asInstanceOf[MdNode.PhrasingContent]), meta.copy(span = Absent))
    case MdNode.Heading(depth, children, meta) =>
      MdNode.Heading(
        depth,
        children.map(_.unpositioned.asInstanceOf[MdNode.PhrasingContent]),
        meta.copy(span = Absent)
      )
    case MdNode.Code(info, value, meta)    => MdNode.Code(info, value, meta.copy(span = Absent))
    case MdNode.Html(value, meta)          => MdNode.Html(value, meta.copy(span = Absent))
    case MdNode.Blockquote(children, meta) =>
      MdNode.Blockquote(children.map(_.unpositioned.asInstanceOf[MdNode.FlowContent]), meta.copy(span = Absent))
    case MdNode.List(ordered, start, spread, children, meta) =>
      MdNode.List(
        ordered,
        start,
        spread,
        children.map(item => item.unpositioned.asInstanceOf[MdNode.ListItem]),
        meta.copy(span = Absent)
      )
    case MdNode.ListItem(children, meta) =>
      MdNode.ListItem(children.map(_.unpositioned.asInstanceOf[MdNode.FlowContent]), meta.copy(span = Absent))
    case MdNode.ThematicBreak(meta)              => MdNode.ThematicBreak(meta.copy(span = Absent))
    case MdNode.Text(value, meta)                => MdNode.Text(value, meta.copy(span = Absent))
    case MdNode.InlineCode(value, meta)          => MdNode.InlineCode(value, meta.copy(span = Absent))
    case MdNode.Link(url, title, children, meta) =>
      MdNode.Link(
        url,
        title,
        children.map(_.unpositioned.asInstanceOf[MdNode.PhrasingContent]),
        meta.copy(span = Absent)
      )
    case MdNode.Image(url, title, alt, meta) => MdNode.Image(url, title, alt, meta.copy(span = Absent))
    case MdNode.Emphasis(children, meta)     =>
      MdNode.Emphasis(children.map(_.unpositioned.asInstanceOf[MdNode.PhrasingContent]), meta.copy(span = Absent))
    case MdNode.Strong(children, meta) =>
      MdNode.Strong(children.map(_.unpositioned.asInstanceOf[MdNode.PhrasingContent]), meta.copy(span = Absent))
    case MdNode.InlineHtml(value, meta)       => MdNode.InlineHtml(value, meta.copy(span = Absent))
    case MdNode.Break(meta)                   => MdNode.Break(meta.copy(span = Absent))
    case MdNode.FrontMatter.Yaml(value, meta) => MdNode.FrontMatter.Yaml(value, meta.copy(span = Absent))

object MdNode:

  /** What a block position may hold — mdast's flow content. */
  type FlowContent = Paragraph | Heading | Code | Html | Blockquote | List | ThematicBreak

  /** What a prose position may hold — mdast's phrasing content. */
  type PhrasingContent = Text | InlineCode | Link | Image | Emphasis | Strong | InlineHtml | Break

  /**
   * Front matter: metadata block a profile recognizes at document start. Cases nest here; Toml/Json are later members.
   */
  sealed trait FrontMatter extends MdNode

  object FrontMatter:
    /** A `---`-delimited YAML block. `value` is the raw document text; decoding belongs to the consumer. */
    final case class Yaml(value: YamlDocText, meta: MdMeta = MdMeta.empty) extends FrontMatter

  // flow
  final case class Root(
      children: Chunk[FlowContent],
      frontmatter: Maybe[FrontMatter] = Absent,
      meta: MdMeta = MdMeta.empty
  ) extends MdNode
  final case class Paragraph(children: Chunk[PhrasingContent], meta: MdMeta = MdMeta.empty) extends MdNode
  final case class Heading(depth: HeadingLevel, children: Chunk[PhrasingContent], meta: MdMeta = MdMeta.empty)
      extends MdNode

  /** Fenced and indented code both: the fence-or-indent distinction is the CST's. Indented code has empty info. */
  final case class Code(info: FenceInfo, value: String, meta: MdMeta = MdMeta.empty) extends MdNode

  /** Block-position raw HTML. Its inline twin is [[InlineHtml]]; both project as Unist `html`. */
  final case class Html(value: String, meta: MdMeta = MdMeta.empty) extends MdNode

  final case class Blockquote(children: Chunk[FlowContent], meta: MdMeta = MdMeta.empty) extends MdNode

  /** `start` is Present only when `ordered`. `spread` is mdast's field; the renderer reads the derived `tight`. */
  final case class List(
      ordered: Boolean,
      start: Maybe[Int],
      spread: Boolean,
      children: Chunk[ListItem],
      meta: MdMeta = MdMeta.empty
  ) extends MdNode

  final case class ListItem(children: Chunk[FlowContent], meta: MdMeta = MdMeta.empty) extends MdNode
  final case class ThematicBreak(meta: MdMeta = MdMeta.empty)                          extends MdNode

  // phrasing
  final case class Text(value: String, meta: MdMeta = MdMeta.empty)       extends MdNode
  final case class InlineCode(value: String, meta: MdMeta = MdMeta.empty) extends MdNode

  /** A link. An autolink lowers here too, its raw URI as both destination and only text child. */
  final case class Link(
      url: String,
      title: Maybe[String],
      children: Chunk[PhrasingContent],
      meta: MdMeta = MdMeta.empty
  ) extends MdNode
  final case class Image(url: String, title: Maybe[String], alt: String, meta: MdMeta = MdMeta.empty) extends MdNode
  final case class Emphasis(children: Chunk[PhrasingContent], meta: MdMeta = MdMeta.empty)            extends MdNode
  final case class Strong(children: Chunk[PhrasingContent], meta: MdMeta = MdMeta.empty)              extends MdNode
  final case class InlineHtml(value: String, meta: MdMeta = MdMeta.empty)                             extends MdNode
  final case class Break(meta: MdMeta = MdMeta.empty)                                                 extends MdNode

  extension (list: List)
    /** The renderer's positive: a tight list drops the `p` from its items. Derived, never stored. */
    def tight: Boolean = !list.spread

  extension [N <: MdNode](node: N)
    /**
     * This node with one more typed annotation on it, at its own type.
     *
     * The result type is the node's own case, not [[MdNode]], which is what lets an annotated node stay in the
     * content-category position it came from — a `Text` annotated is still phrasing content. Each arm rebuilds its own
     * case, so the cast back to `N` restores exactly the type the match tore off.
     */
    def withMeta[A](key: MetaKey[A], value: A): N =
      val updated        = node.meta.updated(key, value)
      val result: MdNode = node match
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

package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * The Markdown AST: one node type, mdast vocabulary, typed containment.
 *
 * The CST records what was written; this tree is what it means, produced only by lowering the CST. One sealed trait
 * gives uniform traversal — every node answers [[span]], [[childNodes]] and [[literal]] — while the content-category
 * unions in the companion keep invalid states unrepresentable the way mdast's own content models do: a paragraph holds
 * phrasing content, never a heading. Cases are final case classes rather than enum cases because an enum case's
 * constructor widens to the enum type, which no union member ever is; a case class constructs at its precise type,
 * which is what lets `Paragraph(Chunk(Text("hi")))` compile. Source-form distinctions (fenced vs indented code, bullet
 * char, list delimiter) live in the CST alone.
 *
 * `Mdc` anticipates Markdown with frontmatter and components: the `Yaml` case joins [[MdcNode.FlowContent]] when intent
 * morphir-lc8.3 lands, and that alias is its one registration point.
 */
sealed trait MdcNode derives CanEqual:

  /** Where this node sits in the source, including its own delimiters; Absent marks a generated node. */
  def span: Maybe[Span]

  /**
   * Every child in document order; empty for leaves. Total, for generic walks.
   *
   * Named `childNodes` (matching `MdcCstNode.childNodes`) rather than `children`: the cases' `children` fields carry
   * narrower category types, and a field may implement a trait member only at the exact same type.
   */
  def childNodes: Chunk[MdcNode] = this match
    case MdcNode.Root(children, _)          => children
    case MdcNode.Paragraph(children, _)     => children
    case MdcNode.Heading(_, children, _)    => children
    case MdcNode.Blockquote(children, _)    => children
    case MdcNode.List(_, _, _, children, _) => children
    case MdcNode.ListItem(children, _)      => children
    case MdcNode.Link(_, _, children, _)    => children
    case MdcNode.Emphasis(children, _)      => children
    case MdcNode.Strong(children, _)        => children
    case _: (MdcNode.Code | MdcNode.Html | MdcNode.ThematicBreak | MdcNode.Text | MdcNode.InlineCode |
          MdcNode.Image | MdcNode.InlineHtml | MdcNode.Break) =>
      Chunk.empty

  /** The literal's text — code content, raw HTML, plain text; Absent for parents and markers. */
  def literal: Maybe[String] = this match
    case MdcNode.Text(value, _)       => Present(value)
    case MdcNode.InlineCode(value, _) => Present(value)
    case MdcNode.Code(_, value, _)    => Present(value)
    case MdcNode.Html(value, _)       => Present(value)
    case MdcNode.InlineHtml(value, _) => Present(value)
    case _                            => Absent

  /** This tree with every span stripped, for structural comparison of generated against parsed trees. */
  def unpositioned: MdcNode = this match
    case MdcNode.Root(children, _) =>
      MdcNode.Root(children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]), Absent)
    case MdcNode.Paragraph(children, _) =>
      MdcNode.Paragraph(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), Absent)
    case MdcNode.Heading(depth, children, _) =>
      MdcNode.Heading(depth, children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), Absent)
    case MdcNode.Code(info, value, _)    => MdcNode.Code(info, value, Absent)
    case MdcNode.Html(value, _)          => MdcNode.Html(value, Absent)
    case MdcNode.Blockquote(children, _) =>
      MdcNode.Blockquote(children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]), Absent)
    case MdcNode.List(ordered, start, spread, children, _) =>
      MdcNode.List(
        ordered,
        start,
        spread,
        children.map(item => item.unpositioned.asInstanceOf[MdcNode.ListItem]),
        Absent
      )
    case MdcNode.ListItem(children, _) =>
      MdcNode.ListItem(children.map(_.unpositioned.asInstanceOf[MdcNode.FlowContent]), Absent)
    case MdcNode.ThematicBreak(_)              => MdcNode.ThematicBreak(Absent)
    case MdcNode.Text(value, _)                => MdcNode.Text(value, Absent)
    case MdcNode.InlineCode(value, _)          => MdcNode.InlineCode(value, Absent)
    case MdcNode.Link(url, title, children, _) =>
      MdcNode.Link(url, title, children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), Absent)
    case MdcNode.Image(url, title, alt, _) => MdcNode.Image(url, title, alt, Absent)
    case MdcNode.Emphasis(children, _)     =>
      MdcNode.Emphasis(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), Absent)
    case MdcNode.Strong(children, _) =>
      MdcNode.Strong(children.map(_.unpositioned.asInstanceOf[MdcNode.PhrasingContent]), Absent)
    case MdcNode.InlineHtml(value, _) => MdcNode.InlineHtml(value, Absent)
    case MdcNode.Break(_)             => MdcNode.Break(Absent)

object MdcNode:

  /** What a block position may hold — mdast's flow content. `Yaml` joins here with morphir-lc8.3. */
  type FlowContent = Paragraph | Heading | Code | Html | Blockquote | List | ThematicBreak

  /** What a prose position may hold — mdast's phrasing content. */
  type PhrasingContent = Text | InlineCode | Link | Image | Emphasis | Strong | InlineHtml | Break

  // flow
  final case class Root(children: Chunk[FlowContent], span: Maybe[Span] = Absent)          extends MdcNode
  final case class Paragraph(children: Chunk[PhrasingContent], span: Maybe[Span] = Absent) extends MdcNode
  final case class Heading(depth: HeadingLevel, children: Chunk[PhrasingContent], span: Maybe[Span] = Absent)
      extends MdcNode

  /** Fenced and indented code both: the fence-or-indent distinction is the CST's. Indented code has empty info. */
  final case class Code(info: FenceInfo, value: String, span: Maybe[Span] = Absent) extends MdcNode

  /** Block-position raw HTML. Its inline twin is [[InlineHtml]]; both project as Unist `html`. */
  final case class Html(value: String, span: Maybe[Span] = Absent) extends MdcNode

  final case class Blockquote(children: Chunk[FlowContent], span: Maybe[Span] = Absent) extends MdcNode

  /** `start` is Present only when `ordered`. `spread` is mdast's field; the renderer reads the derived `tight`. */
  final case class List(
      ordered: Boolean,
      start: Maybe[Int],
      spread: Boolean,
      children: Chunk[ListItem],
      span: Maybe[Span] = Absent
  ) extends MdcNode

  final case class ListItem(children: Chunk[FlowContent], span: Maybe[Span] = Absent) extends MdcNode
  final case class ThematicBreak(span: Maybe[Span] = Absent)                          extends MdcNode

  // phrasing
  final case class Text(value: String, span: Maybe[Span] = Absent)       extends MdcNode
  final case class InlineCode(value: String, span: Maybe[Span] = Absent) extends MdcNode

  /** A link. An autolink lowers here too, its raw URI as both destination and only text child. */
  final case class Link(url: String, title: Maybe[String], children: Chunk[PhrasingContent], span: Maybe[Span] = Absent)
      extends MdcNode
  final case class Image(url: String, title: Maybe[String], alt: String, span: Maybe[Span] = Absent) extends MdcNode
  final case class Emphasis(children: Chunk[PhrasingContent], span: Maybe[Span] = Absent)            extends MdcNode
  final case class Strong(children: Chunk[PhrasingContent], span: Maybe[Span] = Absent)              extends MdcNode
  final case class InlineHtml(value: String, span: Maybe[Span] = Absent)                             extends MdcNode
  final case class Break(span: Maybe[Span] = Absent)                                                 extends MdcNode

  extension (list: List)
    /** The renderer's positive: a tight list drops the `p` from its items. Derived, never stored. */
    def tight: Boolean = !list.spread

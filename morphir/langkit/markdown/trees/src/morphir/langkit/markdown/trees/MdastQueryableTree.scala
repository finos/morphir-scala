package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.{Block, Document, Inline, ListItem}
import morphir.langkit.trees.{FieldName, NodeTypeName, QueryableTree}
import morphir.langkit.trees.unist.{UnistProjection, UnistSpan}

/**
 * The Markdown AST as a queryable tree, in mdast vocabulary.
 *
 * Node type names follow [[https://github.com/syntax-tree/mdast mdast]] — `root`, `heading`, `inlineCode`, `break` — so
 * a Unist projection of this tree is comparable with what remark produces from the same source. Names come from a total
 * match, never from `getClass.getSimpleName`, which is fragile under `fullLinkJS` renaming.
 *
 * The AST has no single node supertype, so the tree's element is the union of the four shapes a document is made of.
 * Traversal starts from a [[Document]], which the union admits directly.
 */
object MdastQueryableTree:

  type MdastNode = Document | Block | ListItem | Inline

  given queryableTree: QueryableTree[MdastNode] with

    def nodeType(t: MdastNode): NodeTypeName = t match
      case _: Document              => NodeTypeName("root")
      case _: Block.Paragraph       => NodeTypeName("paragraph")
      case _: Block.Heading         => NodeTypeName("heading")
      case _: Block.FencedCode      => NodeTypeName("code")
      case _: Block.IndentedCode    => NodeTypeName("code")
      case _: Block.HtmlBlock       => NodeTypeName("html")
      case _: Block.BlockQuote      => NodeTypeName("blockquote")
      case _: Block.UnorderedList   => NodeTypeName("list")
      case _: Block.OrderedList     => NodeTypeName("list")
      case _: Block.ThematicBreak   => NodeTypeName("thematicBreak")
      case _: ListItem              => NodeTypeName("listItem")
      case _: Inline.Text           => NodeTypeName("text")
      case _: Inline.CodeSpan       => NodeTypeName("inlineCode")
      case _: Inline.Link           => NodeTypeName("link")
      case _: Inline.Image          => NodeTypeName("image")
      case _: Inline.Emphasis       => NodeTypeName("emphasis")
      case _: Inline.StrongEmphasis => NodeTypeName("strong")
      case _: Inline.RawHtml        => NodeTypeName("html")
      case _: Inline.LineBreak      => NodeTypeName("break")

    def children(t: MdastNode): Seq[MdastNode] = t match
      case n: Document              => n.blocks
      case n: Block.Paragraph       => n.content
      case n: Block.Heading         => n.content
      case n: Block.BlockQuote      => n.content
      case n: Block.UnorderedList   => n.items
      case n: Block.OrderedList     => n.items
      case n: ListItem              => n.content
      case n: Inline.Link           => n.content
      case n: Inline.Emphasis       => n.content
      case n: Inline.StrongEmphasis => n.content
      case _                        => Seq.empty

    def fields(t: MdastNode): Map[FieldName, Seq[MdastNode]] = t match
      case n: Document              => Map(FieldName("blocks") -> n.blocks)
      case n: Block.Paragraph       => Map(FieldName("content") -> n.content)
      case n: Block.Heading         => Map(FieldName("content") -> n.content)
      case n: Block.BlockQuote      => Map(FieldName("content") -> n.content)
      case n: Block.UnorderedList   => Map(FieldName("items") -> n.items)
      case n: Block.OrderedList     => Map(FieldName("items") -> n.items)
      case n: ListItem              => Map(FieldName("content") -> n.content)
      case n: Inline.Link           => Map(FieldName("content") -> n.content)
      case n: Inline.Emphasis       => Map(FieldName("content") -> n.content)
      case n: Inline.StrongEmphasis => Map(FieldName("content") -> n.content)
      case _                        => Map.empty

    def text(t: MdastNode): Option[String] = t match
      case n: Inline.Text        => Some(n.value)
      case n: Inline.CodeSpan    => Some(n.value)
      case n: Inline.RawHtml     => Some(n.value)
      case n: Inline.Image       => Some(n.alt)
      case n: Block.FencedCode   => Some(n.content)
      case n: Block.IndentedCode => Some(n.content)
      case n: Block.HtmlBlock    => Some(n.content)
      case _                     => None

  given unistProjection: UnistProjection[MdastNode] with
    def span(t: MdastNode): Option[UnistSpan] =
      val nodeSpan = t match
        case n: Document => n.span
        case n: Block    => n.span
        case n: ListItem => n.span
        case n: Inline   => n.span
      Some(UnistSpan(nodeSpan.offset, nodeSpan.end))

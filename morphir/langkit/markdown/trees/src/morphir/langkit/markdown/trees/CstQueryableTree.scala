package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.cst.MdcCstNode
import morphir.langkit.trees.{FieldName, NodeTypeName, QueryableTree}
import morphir.langkit.trees.unist.{UnistProjection, UnistSpan}

/**
 * The Markdown CST as a queryable tree, in its own vocabulary.
 *
 * Names are CST-specific — `atxHeading` and `setextHeading` rather than mdast's one `heading`, `verbatim` and `token`
 * for the leaf kinds — because this view is about what was written, not what it means. Names come from a total match,
 * never from `getClass.getSimpleName`, which is fragile under `fullLinkJS` renaming.
 *
 * Structure lives entirely in child order — the CST's children tile the source — so no named fields are exposed; the
 * AST view is where names like `content` and `items` belong.
 */
object CstQueryableTree:

  given queryableTree: QueryableTree[MdcCstNode] with

    def nodeType(t: MdcCstNode): NodeTypeName = t match
      case _: MdcCstNode.Document                => NodeTypeName("document")
      case _: MdcCstNode.ThematicBreak           => NodeTypeName("thematicBreak")
      case _: MdcCstNode.AtxHeading              => NodeTypeName("atxHeading")
      case _: MdcCstNode.SetextHeading           => NodeTypeName("setextHeading")
      case _: MdcCstNode.FencedCode              => NodeTypeName("fencedCode")
      case _: MdcCstNode.IndentedCode            => NodeTypeName("indentedCode")
      case _: MdcCstNode.Paragraph               => NodeTypeName("paragraph")
      case _: MdcCstNode.BlockQuote              => NodeTypeName("blockQuote")
      case _: MdcCstNode.BulletList              => NodeTypeName("bulletList")
      case _: MdcCstNode.OrderedList             => NodeTypeName("orderedList")
      case _: MdcCstNode.ListItem                => NodeTypeName("listItem")
      case _: MdcCstNode.HtmlBlock               => NodeTypeName("htmlBlock")
      case _: MdcCstNode.LinkReferenceDefinition => NodeTypeName("linkReferenceDefinition")
      case _: MdcCstNode.CodeSpan                => NodeTypeName("codeSpan")
      case _: MdcCstNode.Autolink                => NodeTypeName("autolink")
      case _: MdcCstNode.RawHtml                 => NodeTypeName("rawHtml")
      case _: MdcCstNode.Link                    => NodeTypeName("link")
      case _: MdcCstNode.Image                   => NodeTypeName("image")
      case _: MdcCstNode.Emphasis                => NodeTypeName("emphasis")
      case _: MdcCstNode.HardBreak               => NodeTypeName("hardBreak")
      case _: MdcCstNode.Escape                  => NodeTypeName("escape")
      case _: MdcCstNode.Entity                  => NodeTypeName("entity")
      case _: MdcCstNode.Token                   => NodeTypeName("token")
      case _: MdcCstNode.Text                    => NodeTypeName("text")
      case _: MdcCstNode.Verbatim                => NodeTypeName("verbatim")
      case _: MdcCstNode.PhantomIndent           => NodeTypeName("phantomIndent")

    def children(t: MdcCstNode): Seq[MdcCstNode] = t.childNodes

    def fields(t: MdcCstNode): Map[FieldName, Seq[MdcCstNode]] = Map.empty

    def text(t: MdcCstNode): Option[String] = t.leafText match
      case Present(value) => Some(value)
      case Absent         => None

  given unistProjection: UnistProjection[MdcCstNode] with
    def span(t: MdcCstNode): Option[UnistSpan] =
      Some(UnistSpan(t.span.offset, t.span.end))

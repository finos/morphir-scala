package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.MdCstNode
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

  given queryableTree: QueryableTree[MdCstNode] with

    def nodeType(t: MdCstNode): NodeTypeName = t match
      case _: MdCstNode.Document                => NodeTypeName("document")
      case _: MdCstNode.Frontmatter             => NodeTypeName("frontmatter")
      case _: MdCstNode.ThematicBreak           => NodeTypeName("thematicBreak")
      case _: MdCstNode.AtxHeading              => NodeTypeName("atxHeading")
      case _: MdCstNode.SetextHeading           => NodeTypeName("setextHeading")
      case _: MdCstNode.FencedCode              => NodeTypeName("fencedCode")
      case _: MdCstNode.IndentedCode            => NodeTypeName("indentedCode")
      case _: MdCstNode.Paragraph               => NodeTypeName("paragraph")
      case _: MdCstNode.BlockQuote              => NodeTypeName("blockQuote")
      case _: MdCstNode.BulletList              => NodeTypeName("bulletList")
      case _: MdCstNode.OrderedList             => NodeTypeName("orderedList")
      case _: MdCstNode.ListItem                => NodeTypeName("listItem")
      case _: MdCstNode.HtmlBlock               => NodeTypeName("htmlBlock")
      case _: MdCstNode.LinkReferenceDefinition => NodeTypeName("linkReferenceDefinition")
      case _: MdCstNode.CodeSpan                => NodeTypeName("codeSpan")
      case _: MdCstNode.Autolink                => NodeTypeName("autolink")
      case _: MdCstNode.RawHtml                 => NodeTypeName("rawHtml")
      case _: MdCstNode.Link                    => NodeTypeName("link")
      case _: MdCstNode.Image                   => NodeTypeName("image")
      case _: MdCstNode.Emphasis                => NodeTypeName("emphasis")
      case _: MdCstNode.HardBreak               => NodeTypeName("hardBreak")
      case _: MdCstNode.Escape                  => NodeTypeName("escape")
      case _: MdCstNode.Entity                  => NodeTypeName("entity")
      case _: MdCstNode.Token                   => NodeTypeName("token")
      case _: MdCstNode.Text                    => NodeTypeName("text")
      case _: MdCstNode.Verbatim                => NodeTypeName("verbatim")
      case _: MdCstNode.PhantomIndent           => NodeTypeName("phantomIndent")

    def children(t: MdCstNode): Seq[MdCstNode] = t.childNodes

    def fields(t: MdCstNode): Map[FieldName, Seq[MdCstNode]] = Map.empty

    def text(t: MdCstNode): Option[String] = t.leafText match
      case Present(value) => Some(value)
      case Absent         => None

  given unistProjection: UnistProjection[MdCstNode] with
    def span(t: MdCstNode): Option[UnistSpan] =
      Some(UnistSpan(t.span.offset, t.span.end))

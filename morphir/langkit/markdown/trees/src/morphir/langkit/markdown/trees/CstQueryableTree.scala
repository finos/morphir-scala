package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.cst.CstNode
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

  given queryableTree: QueryableTree[CstNode] with

    def nodeType(t: CstNode): NodeTypeName = t match
      case _: CstNode.Document                => NodeTypeName("document")
      case _: CstNode.ThematicBreak           => NodeTypeName("thematicBreak")
      case _: CstNode.AtxHeading              => NodeTypeName("atxHeading")
      case _: CstNode.SetextHeading           => NodeTypeName("setextHeading")
      case _: CstNode.FencedCode              => NodeTypeName("fencedCode")
      case _: CstNode.IndentedCode            => NodeTypeName("indentedCode")
      case _: CstNode.Paragraph               => NodeTypeName("paragraph")
      case _: CstNode.BlockQuote              => NodeTypeName("blockQuote")
      case _: CstNode.BulletList              => NodeTypeName("bulletList")
      case _: CstNode.OrderedList             => NodeTypeName("orderedList")
      case _: CstNode.ListItem                => NodeTypeName("listItem")
      case _: CstNode.HtmlBlock               => NodeTypeName("htmlBlock")
      case _: CstNode.LinkReferenceDefinition => NodeTypeName("linkReferenceDefinition")
      case _: CstNode.CodeSpan                => NodeTypeName("codeSpan")
      case _: CstNode.Autolink                => NodeTypeName("autolink")
      case _: CstNode.RawHtml                 => NodeTypeName("rawHtml")
      case _: CstNode.Link                    => NodeTypeName("link")
      case _: CstNode.Image                   => NodeTypeName("image")
      case _: CstNode.Emphasis                => NodeTypeName("emphasis")
      case _: CstNode.HardBreak               => NodeTypeName("hardBreak")
      case _: CstNode.Escape                  => NodeTypeName("escape")
      case _: CstNode.Entity                  => NodeTypeName("entity")
      case _: CstNode.Token                   => NodeTypeName("token")
      case _: CstNode.Text                    => NodeTypeName("text")
      case _: CstNode.Verbatim                => NodeTypeName("verbatim")
      case _: CstNode.PhantomIndent           => NodeTypeName("phantomIndent")

    def children(t: CstNode): Seq[CstNode] = t.childNodes

    def fields(t: CstNode): Map[FieldName, Seq[CstNode]] = Map.empty

    def text(t: CstNode): Option[String] = t.leafText match
      case Present(value) => Some(value)
      case Absent         => None

  given unistProjection: UnistProjection[CstNode] with
    def span(t: CstNode): Option[UnistSpan] =
      Some(UnistSpan(t.span.offset, t.span.end))

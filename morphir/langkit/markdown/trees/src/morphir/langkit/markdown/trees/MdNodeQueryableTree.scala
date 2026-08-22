package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.MdNode
import morphir.langkit.trees.{FieldName, NodeTypeName, QueryableTree}
import morphir.langkit.trees.unist.{UnistProjection, UnistSpan}

/**
 * The Markdown AST as a queryable tree, in mdast vocabulary.
 *
 * Node type names follow [[https://github.com/syntax-tree/mdast mdast]] — `root`, `heading`, `inlineCode`, `break` — so
 * a Unist projection of this tree is comparable with what remark produces from the same source. Names come from a total
 * match, never from `getClass.getSimpleName`, which is fragile under `fullLinkJS` renaming.
 *
 * [[MdNode]] gives every case one supertype, so the union alias the old `Document | Block | ListItem | Inline` instance
 * needed is gone: `nodeType`, `children` and `text` read the trait's own accessors (`childNodes`, `literal`) instead of
 * matching per case, and the field name is uniformly `children` because [[MdNode]] carries no per-case field names.
 */
object MdNodeQueryableTree:

  given queryableTree: QueryableTree[MdNode] with

    def nodeType(t: MdNode): NodeTypeName = t match
      case _: MdNode.Root             => NodeTypeName("root")
      case _: MdNode.Paragraph        => NodeTypeName("paragraph")
      case _: MdNode.Heading          => NodeTypeName("heading")
      case _: MdNode.Code             => NodeTypeName("code")
      case _: MdNode.Html             => NodeTypeName("html")
      case _: MdNode.Blockquote       => NodeTypeName("blockquote")
      case _: MdNode.List             => NodeTypeName("list")
      case _: MdNode.ListItem         => NodeTypeName("listItem")
      case _: MdNode.ThematicBreak    => NodeTypeName("thematicBreak")
      case _: MdNode.Table            => NodeTypeName("table")
      case _: MdNode.TableRow         => NodeTypeName("tableRow")
      case _: MdNode.TableCell        => NodeTypeName("tableCell")
      case _: MdNode.Text             => NodeTypeName("text")
      case _: MdNode.InlineCode       => NodeTypeName("inlineCode")
      case _: MdNode.Link             => NodeTypeName("link")
      case _: MdNode.Image            => NodeTypeName("image")
      case _: MdNode.Emphasis         => NodeTypeName("emphasis")
      case _: MdNode.Strong           => NodeTypeName("strong")
      case _: MdNode.Delete           => NodeTypeName("delete")
      case _: MdNode.InlineHtml       => NodeTypeName("html")
      case _: MdNode.Break            => NodeTypeName("break")
      case _: MdNode.FrontMatter.Yaml => NodeTypeName("yaml")

    def children(t: MdNode): Seq[MdNode] = t.childNodes

    def fields(t: MdNode): Map[FieldName, Seq[MdNode]] =
      if t.childNodes.isEmpty then Map.empty else Map(FieldName("children") -> t.childNodes)

    def text(t: MdNode): Option[String] = t.literal match
      case Present(value) => Some(value)
      case Absent         => None

  given unistProjection: UnistProjection[MdNode] with
    def span(t: MdNode): Option[UnistSpan] = t.span match
      case Present(s) => Some(UnistSpan(s.offset, s.end))
      case Absent     => None // generated node: no position, per unist

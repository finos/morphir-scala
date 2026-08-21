package morphir.langkit.markdown.trees

import kyo.*
import morphir.langkit.markdown.MdcNode
import morphir.langkit.trees.{FieldName, NodeTypeName, QueryableTree}
import morphir.langkit.trees.unist.{UnistProjection, UnistSpan}

/**
 * The Markdown AST as a queryable tree, in mdast vocabulary.
 *
 * Node type names follow [[https://github.com/syntax-tree/mdast mdast]] — `root`, `heading`, `inlineCode`, `break` — so
 * a Unist projection of this tree is comparable with what remark produces from the same source. Names come from a total
 * match, never from `getClass.getSimpleName`, which is fragile under `fullLinkJS` renaming.
 *
 * [[MdcNode]] gives every case one supertype, so the union alias the old `Document | Block | ListItem | Inline`
 * instance needed is gone: `nodeType`, `children` and `text` read the trait's own accessors (`childNodes`, `literal`)
 * instead of matching per case, and the field name is uniformly `children` because [[MdcNode]] carries no per-case
 * field names.
 */
object MdcNodeQueryableTree:

  given queryableTree: QueryableTree[MdcNode] with

    def nodeType(t: MdcNode): NodeTypeName = t match
      case _: MdcNode.Root             => NodeTypeName("root")
      case _: MdcNode.Paragraph        => NodeTypeName("paragraph")
      case _: MdcNode.Heading          => NodeTypeName("heading")
      case _: MdcNode.Code             => NodeTypeName("code")
      case _: MdcNode.Html             => NodeTypeName("html")
      case _: MdcNode.Blockquote       => NodeTypeName("blockquote")
      case _: MdcNode.List             => NodeTypeName("list")
      case _: MdcNode.ListItem         => NodeTypeName("listItem")
      case _: MdcNode.ThematicBreak    => NodeTypeName("thematicBreak")
      case _: MdcNode.Text             => NodeTypeName("text")
      case _: MdcNode.InlineCode       => NodeTypeName("inlineCode")
      case _: MdcNode.Link             => NodeTypeName("link")
      case _: MdcNode.Image            => NodeTypeName("image")
      case _: MdcNode.Emphasis         => NodeTypeName("emphasis")
      case _: MdcNode.Strong           => NodeTypeName("strong")
      case _: MdcNode.InlineHtml       => NodeTypeName("html")
      case _: MdcNode.Break            => NodeTypeName("break")
      case _: MdcNode.FrontMatter.Yaml => NodeTypeName("yaml")

    def children(t: MdcNode): Seq[MdcNode] = t.childNodes

    def fields(t: MdcNode): Map[FieldName, Seq[MdcNode]] =
      if t.childNodes.isEmpty then Map.empty else Map(FieldName("children") -> t.childNodes)

    def text(t: MdcNode): Option[String] = t.literal match
      case Present(value) => Some(value)
      case Absent         => None

  given unistProjection: UnistProjection[MdcNode] with
    def span(t: MdcNode): Option[UnistSpan] = t.span match
      case Present(s) => Some(UnistSpan(s.offset, s.end))
      case Absent     => None // generated node: no position, per unist

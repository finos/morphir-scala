package morphir.langkit.markdown.internal

import kyo.*
import morphir.langkit.markdown.*

/**
 * The single owner of Markdown AST traversal.
 *
 * Walks bottom-up: children compile first, then each node combines them into one `Out`. Every output format reuses this
 * walk by supplying a [[morphir.langkit.markdown.Compiler]] instead of writing its own.
 *
 * Internal on purpose. Callers reach it as `Compiler.compile`, which exports the entry point below, so the traversal
 * can gain node kinds as intent 0021 widens the AST without that being a change to the public surface.
 */
private[markdown] object MarkdownFold:

  def compile[Out](root: MdcNode.Root)(using compiler: Compiler[Out]): Out =
    compiler.root(root.children.map(compileFlow))

  private def compileFlow[Out](node: MdcNode.FlowContent)(using compiler: Compiler[Out]): Out =
    node match
      case MdcNode.Heading(depth, children, _) => compiler.heading(depth, children.map(compilePhrasing))
      case MdcNode.Paragraph(children, _)      => compiler.paragraph(children.map(compilePhrasing))
      // One branch for both source forms: the fence-or-indent distinction is the CST's, and an indented block
      // reaches here with an empty info string, which is what CommonMark renders an info-less fence as.
      case MdcNode.Code(info, value, _)                     => compiler.code(info, value)
      case MdcNode.Html(value, _)                           => compiler.html(value)
      case MdcNode.Blockquote(children, _)                  => compiler.blockquote(children.map(compileFlow))
      case list @ MdcNode.List(ordered, start, _, items, _) =>
        compiler.list(ordered, start, items.map(item => compiler.listItem(compileItem(item, list.tight))))
      case MdcNode.ThematicBreak(_) => compiler.thematicBreak

  /**
   * One list item's children, with the newlines CommonMark writes around block-level siblings.
   *
   * Two rules meet here, and neither belongs in a writer. A tight list drops the `p` element from its items' paragraphs
   * -- the element, not the content -- so a tight paragraph contributes its prose directly. And a block-level child is
   * written on its own line, which means a newline before it unless one is already there and a newline after it. That
   * is why `- a` over `  - b` renders `<li>a` then the nested list on the next line, while a loose item's two
   * paragraphs each get a line of their own.
   */
  private def compileItem[Out](item: MdcNode.ListItem, tight: Boolean)(using compiler: Compiler[Out]): Chunk[Out] =
    val out              = List.newBuilder[Out]
    var previousWasBlock = false
    item.children.foreach {
      case MdcNode.Paragraph(children, _) if tight =>
        out ++= children.map(compilePhrasing)
        previousWasBlock = false
      case block =>
        if !previousWasBlock then out += compiler.blockSeparator
        out += compileFlow(block)
        out += compiler.blockSeparator
        previousWasBlock = true
    }
    Chunk.from(out.result())

  private def compilePhrasing[Out](node: MdcNode.PhrasingContent)(using compiler: Compiler[Out]): Out =
    node match
      case MdcNode.Text(value, _)                => compiler.text(value)
      case MdcNode.InlineCode(value, _)          => compiler.inlineCode(value)
      case MdcNode.Link(url, title, children, _) => compiler.link(url, title, children.map(compilePhrasing))
      case MdcNode.Image(url, title, alt, _)     => compiler.image(url, title, alt)
      case MdcNode.Emphasis(children, _)         => compiler.emphasis(children.map(compilePhrasing))
      case MdcNode.Strong(children, _)           => compiler.strong(children.map(compilePhrasing))
      case MdcNode.InlineHtml(value, _)          => compiler.inlineHtml(value)
      case MdcNode.Break(_)                      => compiler.break
end MarkdownFold

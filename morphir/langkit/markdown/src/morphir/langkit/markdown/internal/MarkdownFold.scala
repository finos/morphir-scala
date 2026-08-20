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

  def compile[Out](document: Document)(using compiler: Compiler[Out]): Out =
    compiler.document(document.blocks.map(compileBlock))

  private def compileBlock[Out](block: Block)(using compiler: Compiler[Out]): Out =
    block match
      case Block.Heading(level, content, _)   => compiler.heading(level, content.map(compileInline))
      case Block.Paragraph(content, _)        => compiler.paragraph(content.map(compileInline))
      case Block.FencedCode(info, content, _) => compiler.fencedCode(info, content)
      // Same output as an info-less fence: CommonMark renders both as pre > code.
      case Block.IndentedCode(content, _)       => compiler.fencedCode(FenceInfo.empty, content)
      case Block.UnorderedList(items, tight, _) =>
        compiler.unorderedList(items.map(item => compiler.listItem(compileItem(item, tight))))
      case Block.OrderedList(start, items, tight, _) =>
        compiler.orderedList(start, items.map(item => compiler.listItem(compileItem(item, tight))))
      case Block.HtmlBlock(content, _)  => compiler.htmlBlock(content)
      case Block.BlockQuote(content, _) => compiler.blockQuote(content.map(compileBlock))
      case Block.ThematicBreak(_)       => compiler.thematicBreak

  /**
   * One list item's children, with the newlines CommonMark writes around block-level siblings.
   *
   * Two rules meet here, and neither belongs in a writer. A tight list drops the `p` element from its items' paragraphs
   * -- the element, not the content -- so a tight paragraph contributes its prose directly. And a block-level child is
   * written on its own line, which means a newline before it unless one is already there and a newline after it. That
   * is why `- a` over `  - b` renders `<li>a` then the nested list on the next line, while a loose item's two
   * paragraphs each get a line of their own.
   */
  private def compileItem[Out](item: ListItem, tight: Boolean)(using compiler: Compiler[Out]): Chunk[Out] =
    val children         = List.newBuilder[Out]
    var previousWasBlock = false
    item.content.foreach {
      case Block.Paragraph(content, _) if tight =>
        children ++= content.map(compileInline)
        previousWasBlock = false
      case block =>
        if !previousWasBlock then children += compiler.blockSeparator
        children += compileBlock(block)
        children += compiler.blockSeparator
        previousWasBlock = true
    }
    Chunk.from(children.result())

  private def compileInline[Out](inline0: Inline)(using compiler: Compiler[Out]): Out =
    inline0 match
      case Inline.Text(value, _)                       => compiler.text(value)
      case Inline.CodeSpan(value, _)                   => compiler.codeSpan(value)
      case Inline.Link(destination, title, content, _) =>
        compiler.link(destination, title, content.map(compileInline))
      case Inline.Image(destination, title, alt, _) => compiler.image(destination, title, alt)
      case Inline.Emphasis(content, _)              => compiler.emphasis(content.map(compileInline))
      case Inline.StrongEmphasis(content, _)        => compiler.strongEmphasis(content.map(compileInline))
      case Inline.RawHtml(value, _)                 => compiler.rawHtml(value)
end MarkdownFold

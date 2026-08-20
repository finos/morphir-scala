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
      case Block.IndentedCode(content, _) => compiler.fencedCode(FenceInfo.empty, content)
      case Block.UnorderedList(items, _)  =>
        compiler.unorderedList(items.map(item => compiler.listItem(item.content.map(compileInline))))
      case Block.OrderedList(start, items, _) =>
        compiler.orderedList(start, items.map(item => compiler.listItem(item.content.map(compileInline))))
      case Block.HtmlBlock(content, _) => compiler.htmlBlock(content)
      case Block.ThematicBreak(_)      => compiler.thematicBreak

  private def compileInline[Out](inline0: Inline)(using compiler: Compiler[Out]): Out =
    inline0 match
      case Inline.Text(value, _)                       => compiler.text(value)
      case Inline.CodeSpan(value, _)                   => compiler.codeSpan(value)
      case Inline.Link(destination, title, content, _) =>
        compiler.link(destination, title, content.map(compileInline))
      case Inline.Image(destination, title, alt, _) => compiler.image(destination, title, alt)
      case Inline.Emphasis(content, _)              => compiler.emphasis(content.map(compileInline))
      case Inline.StrongEmphasis(content, _)        => compiler.strongEmphasis(content.map(compileInline))
end MarkdownFold

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
      case Block.UnorderedList(items, _)      =>
        compiler.unorderedList(items.map(item => compiler.listItem(item.content.map(compileInline))))
      case Block.ThematicBreak(_) => compiler.thematicBreak

  private def compileInline[Out](inline0: Inline)(using compiler: Compiler[Out]): Out =
    inline0 match
      case Inline.Text(value, _) => compiler.text(value)
end MarkdownFold

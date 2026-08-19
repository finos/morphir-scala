package morphir.langkit.markdown.compile

import kyo.*
import morphir.langkit.markdown.*

/**
 * A fold over the Markdown AST with one method per node kind, whose children arrive already compiled.
 *
 * Each output format supplies only this mapping; [[MarkdownCompiler]] owns the traversal, so a new target never repeats
 * it. That shared mapping is also the guard that keeps two writers honest: a node the conformance oracle exercises is a
 * node every other target must implement too.
 *
 * A `Monoid[Out]` cannot express this shape. A monoid concatenates siblings, and a heading wraps its children rather
 * than sitting beside them.
 *
 * The algebra stays pure. An effectful target instantiates `Out` at `A < S` — Kyo's pending-effect type — rather than
 * making every method effectful, which would spread the effect across every format and buy nothing.
 *
 * @tparam Out
 *   what this format compiles a node to: a `kyo.UI` value, a ScalaTags `Frag`, plain text, or anything else
 */
trait Compiler[Out]:
  def document(children: Chunk[Out]): Out
  def heading(level: HeadingLevel, text: String): Out
  def paragraph(text: String): Out
  def fencedCode(info: FenceInfo, content: String): Out
  def unorderedList(items: Chunk[Out]): Out
  def listItem(text: String): Out
  def thematicBreak: Out
end Compiler

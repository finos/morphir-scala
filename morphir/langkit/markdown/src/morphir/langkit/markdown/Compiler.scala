package morphir.langkit.markdown

import kyo.*

/**
 * A fold over the Markdown AST with one method per node kind, whose children arrive already compiled.
 *
 * Each output format supplies only this mapping; the traversal is owned once, in
 * [[morphir.langkit.markdown.internal.MarkdownFold]], so a new target never repeats it. That shared mapping is also the
 * structural guard two writers rely on: a node the CommonMark conformance oracle exercises is a node every other target
 * must implement too.
 *
 * The algebra stays pure. An effectful target instantiates `Out` at `A < S` — Kyo's pending-effect type — rather than
 * making every method effectful.
 *
 * The rules for adding a node kind, and the shapes considered and rejected for this stage, are in this module's
 * `CONTRIBUTING.md`.
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

object Compiler:
  /**
   * Compile a whole document with the given output format.
   *
   * Re-exported from the internal fold so callers reach the traversal through the algebra they already have in scope,
   * and the walk itself stays an implementation detail.
   */
  export morphir.langkit.markdown.internal.MarkdownFold.compile
end Compiler

package morphir.langkit.markdown

import kyo.*

/**
 * A fold over the Markdown AST with one method per node kind, whose children arrive already compiled.
 *
 * Each output format supplies only this mapping; the traversal is owned once and shared, so a new target never repeats
 * it. That shared mapping is also the structural guard two writers rely on: a node the CommonMark conformance oracle
 * exercises is a node every other target must implement too.
 *
 * The algebra stays pure. An effectful target instantiates `Out` at `A < S` — Kyo's pending-effect type — rather than
 * making every method effectful.
 *
 * Prose reaches a writer as compiled children, not as a `String`: a block that holds prose folds its inline content
 * first, and [[text]] is the leaf that carries the literal runs. Every `String` this algebra hands you is raw source
 * text, never escaped. **Escaping is the writer's job.**
 *
 * The rules for adding a node kind, and the shapes considered and rejected for this stage, are in this module's
 * `CONTRIBUTING.md`.
 *
 * @tparam Out
 *   what this format compiles a node to: a `kyo.UI` value, a ScalaTags `Frag`, plain text, or anything else
 */
trait Compiler[Out]:

  /**
   * Combine the document's compiled blocks into the whole output.
   *
   * This is the last call of any compile and the only one guaranteed to happen, including for an empty document, where
   * `children` is empty.
   *
   * @param children
   *   the compiled top-level blocks, in source order
   */
  def document(children: Chunk[Out]): Out

  /**
   * Compile a heading from its compiled inline content.
   *
   * @param level
   *   one to six; [[HeadingLevel]] makes a level CommonMark cannot express unrepresentable, so no range check is needed
   * @param children
   *   the compiled inline content, in source order
   */
  def heading(level: HeadingLevel, children: Chunk[Out]): Out

  /** Compile a paragraph from its compiled inline content, in source order. */
  def paragraph(children: Chunk[Out]): Out

  /**
   * Compile a fenced code block.
   *
   * @param info
   *   the parsed fence info string. CommonMark treats it as opaque, so read `info.language` for the first bare token
   *   rather than the whole of `info.raw`; it is [[kyo.Absent]] for a fence that names no language
   * @param content
   *   the code between the fences, verbatim and un-indented. Emit it unchanged apart from escaping.
   *
   * Mind the trailing newline, which is a property of the source rather than a guarantee: a closed, non-empty fence
   * ends with `\n`, an empty one yields `""`, and a fence left unterminated at end of input keeps only the newline its
   * last line actually had. CommonMark's expected HTML always closes the block with one, so a writer aiming at
   * byte-exact conformance has to reckon with the unterminated case rather than assume it
   */
  def fencedCode(info: FenceInfo, content: String): Out

  /**
   * Combine compiled list items into a bullet list.
   *
   * @param items
   *   the results of [[listItem]], in source order
   */
  def unorderedList(items: Chunk[Out]): Out

  /** Compile one bullet-list item from its compiled inline content. Called before [[unorderedList]]. */
  def listItem(children: Chunk[Out]): Out

  /**
   * Compile a run of literal text.
   *
   * The value is raw source text: never escaped, and never further parsed. **Escaping is the writer's job.** Inline
   * markers that no other case claims yet — emphasis, links, code spans — still sit unparsed inside it.
   */
  def text(value: String): Out

  /** Compile a thematic break. It has no children and no text, so this is a constant for most formats. */
  def thematicBreak: Out
end Compiler

object Compiler:
  /**
   * Compile a whole document with the given output format.
   *
   * Walks bottom-up: children compile first, then each node combines them into one `Out`. The walk itself is an
   * implementation detail, so every format reuses it by supplying a [[Compiler]] rather than writing its own.
   */
  export morphir.langkit.markdown.internal.MarkdownFold.compile
end Compiler

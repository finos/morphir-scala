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
 * Every `String` this algebra hands you is raw source text, never escaped and never further parsed. **Escaping is the
 * writer's job.** The AST carries no inline nodes yet, so emphasis, links and code spans still sit unparsed inside that
 * text; intent 0021 replaces those `String` parameters with compiled children, which will be a breaking change to this
 * trait.
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
   * Compile a heading.
   *
   * @param level
   *   one to six; [[HeadingLevel]] makes a level CommonMark cannot express unrepresentable, so no range check is needed
   * @param text
   *   the heading text: the leading `#` run and its separating space removed, then trimmed at both ends
   */
  def heading(level: HeadingLevel, text: String): Out

  /**
   * Compile a paragraph.
   *
   * @param text
   *   the paragraph text, trimmed at both ends. It may still contain `\n`, because a soft line break inside a paragraph
   *   is kept verbatim rather than collapsed to a space
   */
  def paragraph(text: String): Out

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

  /** Compile one bullet-list item from its raw text. Called before [[unorderedList]], which receives the results. */
  def listItem(text: String): Out

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

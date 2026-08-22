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
 * The method names are mdast's, because [[MdNode]] is: one [[code]] for both source forms of a code block, one [[list]]
 * reading `ordered` rather than a method per bullet style. A source-form distinction a target wants back is the CST's
 * to answer, not this algebra's.
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
  def root(children: Chunk[Out]): Out

  /**
   * Compile a heading from its compiled inline content.
   *
   * @param depth
   *   one to six; [[HeadingLevel]] makes a level CommonMark cannot express unrepresentable, so no range check is needed
   * @param children
   *   the compiled inline content, in source order
   */
  def heading(depth: HeadingLevel, children: Chunk[Out]): Out

  /** Compile a paragraph from its compiled inline content, in source order. */
  def paragraph(children: Chunk[Out]): Out

  /**
   * Compile a code block, fenced or indented alike.
   *
   * One method for both source forms, because CommonMark renders both as `pre > code` and the AST keeps only what they
   * mean. An indented block arrives with [[FenceInfo.empty]], which is the same thing a fence naming no language
   * arrives with; a target that has to tell the two apart is asking the CST a question, not this algebra.
   *
   * @param info
   *   the parsed fence info string. CommonMark treats it as opaque, so read `info.language` for the first bare token
   *   rather than the whole of `info.raw`; it is [[kyo.Absent]] for a fence that names no language
   * @param value
   *   the code between the fences, verbatim and un-indented. Emit it unchanged apart from escaping.
   *
   * Mind the trailing newline, which is a property of the source rather than a guarantee: a closed, non-empty fence
   * ends with `\n`, an empty one yields `""`, and a fence left unterminated at end of input keeps only the newline its
   * last line actually had. CommonMark's expected HTML always closes the block with one, so a writer aiming at
   * byte-exact conformance has to reckon with the unterminated case rather than assume it
   */
  def code(info: FenceInfo, value: String): Out

  /**
   * Emit a raw HTML block verbatim.
   *
   * The only method on this algebra that must not escape its argument. A target that cannot express raw markup — a
   * plain-text writer, say — should render it as literal text rather than pretend.
   */
  def html(value: String): Out

  /**
   * Compile a block quote from its already-compiled blocks.
   *
   * Takes blocks rather than prose, which is what makes this the first method of the algebra whose children are
   * themselves block output. A target with no notion of quoting still has to produce something here; wrapping the
   * children unchanged is the honest answer.
   */
  def blockquote(children: Chunk[Out]): Out

  /**
   * Combine compiled list items into a list, bullet or numbered.
   *
   * @param ordered
   *   whether the items were written with number markers rather than bullets
   * @param start
   *   the first marker's number, present only for an ordered list, and bounded by what CommonMark's marker spells. HTML
   *   omits the `start` attribute when it is 1.
   * @param children
   *   the results of [[listItem]], in source order
   */
  def list(ordered: Boolean, start: Maybe[ListStart], children: Chunk[Out]): Out

  /**
   * Compile one list item from its compiled children. Called before [[list]].
   *
   * `checked` is Present only for a GFM task list item. A target with no checkbox to render should still say something
   * — the marker was written — rather than silently drop it.
   */
  def listItem(checked: Maybe[Boolean], children: Chunk[Out]): Out

  /** Compile a thematic break. It has no children and no text, so this is a constant for most formats. */
  def thematicBreak: Out

  /**
   * Combine a table's compiled header row and body rows.
   *
   * The header arrives separately rather than as the first entry of `rows`, matching the AST: GFM requires a header,
   * and a writer should not have to trust that a list is non-empty to find it. `align` has one entry per column, and
   * every row has exactly that many cells.
   */
  def table(align: Chunk[Maybe[ColumnAlignment]], header: Out, rows: Chunk[Out]): Out

  /**
   * Compile one table row from its compiled cells.
   *
   * `header` says which half of the table the row is in. The fold hands [[table]] rows that are already compiled, so a
   * row that had to be a `th` rather than a `td` must know it here rather than there.
   */
  def tableRow(header: Boolean, children: Chunk[Out]): Out

  /** Compile one table cell. `alignment` is Absent for a column whose delimiter set none. */
  def tableCell(alignment: Maybe[ColumnAlignment], header: Boolean, children: Chunk[Out]): Out

  /**
   * Compile a run of literal text.
   *
   * The value is raw source text: never escaped, and never further parsed. **Escaping is the writer's job.**
   */
  def text(value: String): Out

  /**
   * Compile a code span.
   *
   * The value is literal text the spec has already normalised, so emit it unchanged apart from escaping. No inline
   * construct inside it is live: a backslash does not escape, and a backtick is just a backtick.
   */
  def inlineCode(value: String): Out

  /**
   * Compile a link from its compiled label content.
   *
   * `url` is already URI-normalised; emit it as an attribute value and let escaping do the rest.
   */
  def link(url: String, title: Maybe[String], children: Chunk[Out]): Out

  /** Compile an image. `alt` is plain text, as the attribute requires. */
  def image(url: String, title: Maybe[String], alt: String): Out

  /** Compile emphasis from its compiled content. */
  def emphasis(children: Chunk[Out]): Out

  /** Compile strong emphasis from its compiled content. */
  def strong(children: Chunk[Out]): Out

  /**
   * Compile strikethrough from its compiled content.
   *
   * Present on the algebra unconditionally, even though only a GFM-profile parse produces the node. A writer that
   * cannot express it should render its children unchanged rather than drop them: the text was written to be read.
   */
  def delete(children: Chunk[Out]): Out

  /**
   * Compile raw HTML written inside prose.
   *
   * The inline counterpart of [[html]], and it carries the same warning: `value` is emitted verbatim, never escaped. A
   * target that cannot emit HTML has to decide what to do with it -- dropping it is safer than escaping it, which would
   * show the author their own markup.
   */
  def inlineHtml(value: String): Out

  /**
   * Compile a hard line break.
   *
   * The break the author asked for, as against the soft one that any line ending gives. A target that reflows its own
   * text still has to honour this one, because it is a request rather than an artefact of how the source was wrapped.
   */
  def break: Out

  /**
   * The newline CommonMark writes between block-level siblings inside a list item.
   *
   * A list item's children may be blocks, prose, or both: a tight item's paragraph is written bare, and a code block
   * beside it is not. Only the traversal knows which children are which, and only the writer knows what a newline is in
   * its own output type, so the two meet here. A target for which the distinction is meaningless can return whatever
   * its empty value is.
   */
  def blockSeparator: Out
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

package morphir.langkit.markdown

import _root_.scalatags.Text.all.*
import kyo.*

/**
 * Compiles the Markdown AST to HTML that matches the CommonMark fixtures byte for byte.
 *
 * This is the conformance oracle. ScalaTags escapes `&`, `<`, `>` and `"` and leaves `'` literal, which is the spec's
 * rule exactly, and spells void tags `<hr />` as the fixtures do — so no canonicalization pass sits between our output
 * and an expected-HTML comparison. Morphir writes no HTML here: only the node mapping, and the block separators the
 * spec requires.
 *
 * The kyo-ui writer cannot serve this role — it stamps a `data-kyo-path` attribute on every element and has no `em` or
 * `strong` element at all. Both writers fold the same [[morphir.langkit.markdown.Compiler]], which is what keeps their
 * node mappings from drifting apart. See
 * [[https://github.com/finos/morphir-scala/blob/main/kb/bundles/intent/0033-markdown-compilation.md intent 0033]].
 *
 * This ships in its own artifact, `morphir-langkit-markdown-scalatags`, but lands its public type in
 * `morphir.langkit.markdown` alongside the algebra it implements, so a caller imports one package to parse and render.
 * ScalaTags is imported as `_root_.scalatags` to keep that unambiguous.
 */
object ScalatagsCompiler:

  /** The block separator CommonMark requires between sibling blocks. */
  private val newline: Frag = "\n"

  given instance: Compiler[Frag] with

    /**
     * CommonMark puts every block on its own line, and ScalaTags concatenates siblings with no separator, so the
     * newline between blocks is the compiler's job rather than the writer's.
     */
    def document(children: Chunk[Frag]): Frag =
      frag(children.toSeq.flatMap(child => Seq(child, newline))*)

    def heading(level: HeadingLevel, children: Chunk[Frag]): Frag =
      tag(s"h${level.toInt}")(frag(children.toSeq*))

    def paragraph(children: Chunk[Frag]): Frag = p(frag(children.toSeq*))

    /**
     * The info string's first bare token becomes `class="language-…"`, and a fence naming no language gets no class at
     * all.
     *
     * `content` is emitted verbatim apart from escaping — no newline is appended. A closed, non-empty fence already
     * carries its trailing newline from the parser, and an empty one must not gain one: spec example 130 expects
     * `<pre><code></code></pre>`.
     */
    def fencedCode(info: FenceInfo, content: String): Frag =
      info.language match
        case Present(language) => pre(code(cls := s"language-$language")(content))
        case Absent            => pre(code(content))

    def unorderedList(items: Chunk[Frag]): Frag =
      ul(newline, frag(items.toSeq.flatMap(item => Seq(item, newline))*))

    def listItem(children: Chunk[Frag]): Frag = li(frag(children.toSeq*))

    /** ScalaTags escapes a `String` frag on render, which is exactly the spec's rule, so nothing is done here. */
    def text(value: String): Frag = value

    def codeSpan(value: String): Frag = code(value)

    def thematicBreak: Frag = hr
  end instance

  /** Compile a document to the HTML the CommonMark fixtures expect. */
  def render(document: Document): String =
    Compiler.compile[Frag](document)(using instance).render
end ScalatagsCompiler

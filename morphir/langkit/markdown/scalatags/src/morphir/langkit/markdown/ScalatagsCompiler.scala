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
    def root(children: Chunk[Frag]): Frag =
      frag(children.toSeq.flatMap(child => Seq(child, newline))*)

    def heading(depth: HeadingLevel, children: Chunk[Frag]): Frag =
      tag(s"h${depth.toInt}")(frag(children.toSeq*))

    def paragraph(children: Chunk[Frag]): Frag = p(frag(children.toSeq*))

    /**
     * The info string's first bare token becomes `class="language-…"`, and a fence naming no language gets no class at
     * all.
     *
     * `value` is emitted verbatim apart from escaping — no newline is appended. A closed, non-empty fence already
     * carries its trailing newline from the parser, and an empty one must not gain one: spec example 130 expects
     * `<pre><code></code></pre>`.
     */
    def code(info: FenceInfo, value: String): Frag =
      info.language match
        case Present(language) => pre(_root_.scalatags.Text.all.code(cls := s"language-$language")(value))
        case Absent            => pre(_root_.scalatags.Text.all.code(value))

    /**
     * Combines `list`'s already-compiled `listItem` children into `ul` when `!ordered`, or `ol` when `ordered`. HTML
     * omits the `start` attribute when it is absent or `1`, exactly as the two former methods this merges did.
     */
    def list(ordered: Boolean, start: Maybe[ListStart], children: Chunk[Frag]): Frag =
      val body = frag(children.toSeq.flatMap(item => Seq(item, newline))*)
      if !ordered then ul(newline, body)
      else
        start match
          case Present(value) if value != ListStart.One => ol(attr("start") := value.toInt.toString)(newline, body)
          case _                                        => ol(newline, body)

    def listItem(children: Chunk[Frag]): Frag = li(frag(children.toSeq*))

    /** ScalaTags escapes a `String` frag on render, which is exactly the spec's rule, so nothing is done here. */
    def text(value: String): Frag = value

    def inlineCode(value: String): Frag = _root_.scalatags.Text.all.code(value)

    def emphasis(children: Chunk[Frag]): Frag = em(frag(children.toSeq*))

    def strong(children: Chunk[Frag]): Frag = _root_.scalatags.Text.all.strong(frag(children.toSeq*))

    def delete(children: Chunk[Frag]): Frag = del(frag(children.toSeq*))

    /** Attribute order follows the fixtures: `href` then `title`, and no `title` attribute at all when absent. */
    def link(url: String, title: Maybe[String], children: Chunk[Frag]): Frag =
      title match
        case Present(value) => a(href := url, scalatags.Text.all.title := value)(frag(children.toSeq*))
        case Absent         => a(href := url)(frag(children.toSeq*))

    def image(url: String, title: Maybe[String], alt: String): Frag =
      title match
        case Present(value) => img(src := url, attr("alt") := alt, scalatags.Text.all.title := value)
        case Absent         => img(src := url, attr("alt") := alt)

    /** `raw` is correct here and in `inlineHtml`, and nowhere else: the content is HTML the document wrote. */
    def html(value: String): Frag = raw(value)

    def inlineHtml(value: String): Frag = raw(value)

    /**
     * ScalaTags spells the element `<br />`, which is what the fixtures expect, and the line ending after it is the
     * writer's to add for the same reason the separators between blocks are: the author's line ending was spent saying
     * that the break was wanted.
     */
    def break: Frag = frag(br, newline)

    /**
     * Every child on its own line, and a newline after the opening tag even when there are none: the fixtures spell an
     * empty quote `<blockquote>\n</blockquote>`.
     */
    def blockquote(children: Chunk[Frag]): Frag =
      _root_.scalatags.Text.all.blockquote(newline, frag(children.toSeq.flatMap(child => Seq(child, newline))*))

    def blockSeparator: Frag = newline

    def thematicBreak: Frag = hr
  end instance

  /** Compile a document to the HTML the CommonMark fixtures expect. */
  def render(root: MdNode.Root): String =
    Compiler.compile[Frag](root)(using instance).render
end ScalatagsCompiler

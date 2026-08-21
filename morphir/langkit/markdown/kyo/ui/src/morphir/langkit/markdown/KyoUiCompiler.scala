package morphir.langkit.markdown

import kyo.*

/**
 * Compiles the Markdown AST to `kyo.UI` values, the markup the morphir-ui client and the desktop app mount.
 *
 * This is the writer users see, and deliberately not the conformance oracle. kyo-ui stamps a `data-kyo-path` attribute
 * onto every element and has no `em` or `strong` element at all, so its HTML cannot be compared byte for byte against
 * the CommonMark fixtures. The ScalaTags writer carries that duty; both fold the same
 * [[morphir.langkit.markdown.Compiler]], which is what keeps their node mappings from drifting apart. See
 * [[https://github.com/finos/morphir-scala/blob/main/kb/bundles/intent/0033-markdown-compilation.md intent 0033]].
 *
 * This ships in its own artifact, `morphir-langkit-markdown-kyo-ui`, but lands its public type in
 * `morphir.langkit.markdown` alongside the algebra it implements, so a caller imports one package to parse and compile.
 * The nested `kyo/ui` module directory is what gives the artifact its `kyo-ui` suffix; it says nothing about the
 * package, which deliberately has no `kyo` segment to shadow Kyo's own root package.
 *
 * kyo-ui is compiled for Java 25, so consumers of this artifact need that runtime.
 */
object KyoUiCompiler:

  given instance: Compiler[UI] with

    def root(children: Chunk[UI]): UI = UI.fragment(children.toSeq*)

    def heading(depth: HeadingLevel, children: Chunk[UI]): UI =
      val body = content(children)
      depth.toInt match
        case 1 => UI.h1(body)
        case 2 => UI.h2(body)
        case 3 => UI.h3(body)
        case 4 => UI.h4(body)
        case 5 => UI.h5(body)
        case _ => UI.h6(body)

    def paragraph(children: Chunk[UI]): UI = UI.p(content(children))

    /** The fence's language becomes `class="language-…"` on the inner `code`, which is what CommonMark's HTML does. */
    def code(info: FenceInfo, value: String): UI =
      val codeElement = info.language match
        case Present(language) => UI.code(value).cssClass(s"language-$language")
        case Absent            => UI.code(value)
      UI.pre(codeElement)

    /**
     * Combines `list`'s already-compiled `listItem` children into `ul` when `!ordered`, or `ol` when `ordered`.
     *
     * `start` is dropped either way, because kyo-ui cannot carry it: `Ast.Ol` is an attribute set and children, the
     * attribute set is a closed one, and neither holds `start`. A list beginning at three therefore renders beginning
     * at one. The AST keeps the number, so nothing is lost that a writer able to express it could not recover.
     */
    def list(ordered: Boolean, start: Maybe[Int], children: Chunk[UI]): UI =
      if ordered then UI.ol(content(children)) else UI.ul(content(children))

    def listItem(children: Chunk[UI]): UI = UI.li(content(children))

    /** kyo-ui escapes a `Text` node when it renders, so the value is handed over raw. */
    def text(value: String): UI = UI.Ast.Text(value)

    def inlineCode(value: String): UI = UI.code(value)

    /**
     * kyo-ui has no `em` or `strong` element, so emphasis is carried by a `span` with a class. The ScalaTags oracle
     * emits the real elements; this path is what users see, and it is the gap intent 0033 recorded.
     */
    def emphasis(children: Chunk[UI]): UI = UI.span(content(children)).cssClass("md-em")

    def strong(children: Chunk[UI]): UI = UI.span(content(children)).cssClass("md-strong")

    /**
     * `Href.Path` and `ImgSrc.Path` render their value verbatim, which is what a Markdown destination needs: it is an
     * arbitrary URI the parser has already normalised, not something to re-classify as absolute, path or fragment.
     *
     * `title` is dropped for the same reason `start` is above: `Ast.Anchor` carries an attribute set, children, an href
     * and a target, and `Ast.Img` a source and an alt. There is no `title` among them and no way to add one -- the
     * attribute set is closed, and `aria`, `data`, `role` and `cssClass` are the only names it opens up. So
     * `[docs](/docs "Guide")` renders without its tooltip. The AST keeps the title either way.
     */
    def link(url: String, title: Maybe[String], children: Chunk[UI]): UI =
      UI.a.href(UI.Href.Path(url))(content(children))

    def image(url: String, title: Maybe[String], alt: String): UI =
      UI.img(UI.ImgSrc.Path(url), alt)

    /** `UI.rawHtml` is correct here and in `inlineHtml`, and nowhere else: the content is HTML the document wrote. */
    def html(value: String): UI = UI.rawHtml(value)

    def inlineHtml(value: String): UI = UI.rawHtml(value)

    def break: UI = UI.fragment(UI.br, UI.Ast.Text("\n"))

    def blockquote(children: Chunk[UI]): UI = UI.blockquote(content(children))

    def blockSeparator: UI = UI.Ast.Text("\n")

    def thematicBreak: UI = UI.hr
  end instance

  /**
   * Gather compiled children into one value an element accepts.
   *
   * A `Fragment` renders its children with no wrapper element of its own, so this adds nothing to the output. It exists
   * because kyo-ui's element builders take a varargs of checked children, and a `Chunk[UI]` cannot be splatted into
   * that position — each argument is witnessed individually at the call site.
   */
  private def content(children: Chunk[UI])(using Frame): UI = UI.fragment(children.toSeq*)

  /**
   * Compile a document to a `kyo.UI` value tree.
   *
   * Nothing is rendered here. The caller decides: `UI.runRender` for a fragment, `UI.runRenderPage` for a whole
   * document, or mounting it live in a browser.
   */
  def compile(root: MdNode.Root): UI =
    Compiler.compile[UI](root)(using instance)
end KyoUiCompiler

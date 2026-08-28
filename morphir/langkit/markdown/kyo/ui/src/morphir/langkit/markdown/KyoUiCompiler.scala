package morphir.langkit.markdown

import kyo.*

/**
 * Compiles the Markdown AST to `kyo.UI` values, the markup the morphir-ui client mounts.
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
    def list(ordered: Boolean, start: Maybe[ListStart], children: Chunk[UI]): UI =
      if ordered then UI.ol(content(children)) else UI.ul(content(children))

    /**
     * A checked box is Present only for a GFM task list item; `checked` says whether it was ticked.
     *
     * kyo-ui's own checkbox is a live input a user could toggle, which is not what a rendered task list is: it is a
     * record of what the author wrote, so it is spelled as raw, disabled markup the way the specification's own
     * rendering is, rather than through kyo-ui's interactive element.
     */
    def listItem(checked: Maybe[Boolean], children: Chunk[UI]): UI =
      checked match
        case Absent          => UI.li(content(children))
        case Present(ticked) =>
          val box =
            if ticked then UI.rawHtml("""<input checked="" disabled="" type="checkbox">""")
            else UI.rawHtml("""<input disabled="" type="checkbox">""")
          UI.li(UI.fragment(box, UI.Ast.Text(" "), content(children)))

    /** kyo-ui escapes a `Text` node when it renders, so the value is handed over raw. */
    def text(value: String): UI = UI.Ast.Text(value)

    def inlineCode(value: String): UI = UI.code(value)

    /**
     * kyo-ui has no `em` or `strong` element, so emphasis is carried by a `span` with a class. The ScalaTags oracle
     * emits the real elements; this path is what users see, and it is the gap intent 0033 recorded.
     */
    def emphasis(children: Chunk[UI]): UI = UI.span(content(children)).cssClass("md-em")

    def strong(children: Chunk[UI]): UI = UI.span(content(children)).cssClass("md-strong")

    /** kyo-ui has no `del` element, so strikethrough is carried by a `span` with a class, as emphasis is. */
    def delete(children: Chunk[UI]): UI = UI.span(content(children)).cssClass("md-del")

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

    /** kyo-ui carries alignment as a class, since its attribute set is closed and holds no `align`. */
    private def alignClass(alignment: Maybe[ColumnAlignment]): String = alignment match
      case Absent                          => ""
      case Present(ColumnAlignment.Left)   => "md-align-left"
      case Present(ColumnAlignment.Right)  => "md-align-right"
      case Present(ColumnAlignment.Center) => "md-align-center"

    /**
     * kyo-ui has `table`, `tbody`, `tr`, `th` and `td`, but no `thead`, so the header row goes in a `tbody` of its own
     * marked `md-thead`, and the body rows go in a second one.
     *
     * Two shapes were rejected for the missing element. A `div` between `table` and `tr` is not a legal table child,
     * and every browser hoists it out of the table, which visibly breaks the rendering. A bare `tr` directly under the
     * `table` is legal, but kyo-ui advises against it in exactly the case this writer is for: [[kyo.UI.Ast.Tbody]]
     * records that a browser synthesizes an implicit `tbody` only while *parsing* row content, and that rows patched
     * into a live table -- which is what a reactive mount does -- become direct `table` children instead, which
     * "renders fine but breaks `tbody`-scoped selectors and semantics". A real row group is what it asks for, so that
     * is what both halves get.
     *
     * What is lost is the head-versus-body distinction itself. `md-thead` names the header group for a stylesheet, but
     * a `tbody` is not a `thead`, so the semantics assistive technology reads off the real element are gone. This is
     * the same class of gap as the missing `em` and the dropped list `start`, and it is recorded here for the same
     * reason.
     *
     * A table with no body rows gets no second group at all, matching what the specification's own rendering does with
     * `tbody` there.
     */
    def table(align: Chunk[Maybe[ColumnAlignment]], header: UI, rows: Chunk[UI]): UI =
      val head = UI.tbody(header).cssClass("md-thead")
      if rows.isEmpty then UI.table(head) else UI.table(head, UI.tbody(content(rows)))

    /**
     * A row is a `tr` in either half of the table, so `header` says nothing here: which of `th` and `td` the cells take
     * is [[tableCell]]'s decision, and which row group the row lands in is [[table]]'s.
     */
    def tableRow(header: Boolean, children: Chunk[UI]): UI = UI.tr(content(children))

    def tableCell(alignment: Maybe[ColumnAlignment], header: Boolean, children: Chunk[UI]): UI =
      val cell = if header then UI.th(content(children)) else UI.td(content(children))
      val name = alignClass(alignment)
      if name.isEmpty then cell else cell.cssClass(name)
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

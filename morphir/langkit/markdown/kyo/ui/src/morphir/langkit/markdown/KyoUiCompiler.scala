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

    def document(children: Chunk[UI]): UI = UI.fragment(children.toSeq*)

    def heading(level: HeadingLevel, text: String): UI =
      level.toInt match
        case 1 => UI.h1(text)
        case 2 => UI.h2(text)
        case 3 => UI.h3(text)
        case 4 => UI.h4(text)
        case 5 => UI.h5(text)
        case _ => UI.h6(text)

    def paragraph(text: String): UI = UI.p(text)

    /** The fence's language becomes `class="language-…"` on the inner `code`, which is what CommonMark's HTML does. */
    def fencedCode(info: FenceInfo, content: String): UI =
      val codeElement = info.language match
        case Present(language) => UI.code(content).cssClass(s"language-$language")
        case Absent            => UI.code(content)
      UI.pre(codeElement)

    def unorderedList(items: Chunk[UI]): UI =
      items.foldLeft(UI.ul)((list, item) => list(item))

    def listItem(text: String): UI = UI.li(text)

    def thematicBreak: UI = UI.hr
  end instance

  /**
   * Compile a document to a `kyo.UI` value tree.
   *
   * Nothing is rendered here. The caller decides: `UI.runRender` for a fragment, `UI.runRenderPage` for a whole
   * document, or mounting it live in a browser.
   */
  def compile(document: Document): UI =
    Compiler.compile[UI](document)(using instance)
end KyoUiCompiler

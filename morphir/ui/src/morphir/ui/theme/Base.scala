package morphir.ui.theme

import kyo.*
import kyo.Style
import kyo.Style.FontFamily

/**
 * Document-level base rules. Typed where the `Style` vocabulary reaches; the true global resets (universal box-sizing,
 * selection, scrollbar chrome) live in [[rawCss]] because `Style` has no universal-selector or scrollbar vocabulary at
 * RC6.
 */
object Base:

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Selector.tag("body"),
        Style
          .bg(Tokens.cssVar("bg"))
          .color(Tokens.cssVar("text"))
          .fontSize(14.px)
          .lineHeight(1.55)
          .fontFamily(FontFamily.Custom(Tokens.sansFont))
      )

  val rawCss: String =
    """/* Global resets the typed Style vocabulary cannot express (universal selector, scrollbar
      | * pseudo-elements, font smoothing). Keep this block minimal; everything else is typed. */
      |* { box-sizing: border-box; margin: 0; padding: 0; }
      |html, body { height: 100%; }
      |body { -webkit-font-smoothing: antialiased; }
      |::selection { background: rgba(214, 64, 159, 0.35); }
      |::-webkit-scrollbar { width: 10px; }
      |::-webkit-scrollbar-thumb { background: #2a2438; border-radius: 5px; }
      |""".stripMargin

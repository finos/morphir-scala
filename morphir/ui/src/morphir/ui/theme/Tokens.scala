package morphir.ui.theme

import kyo.*
import kyo.Style.Color

/**
 * The design tokens of the morphir client theme, emitted once as CSS custom properties and referenced from typed rules
 * via [[cssVar]]. One place to change the palette; `scopedVars` on a `data-theme` selector is the future door to
 * alternate themes.
 */
object Tokens:

  val bg        = "#0f0d14"
  val surface   = "#16131d"
  val panel     = "#1a1622"
  val panelEdge = "#2a2438"
  val text      = "#e8e4f1"
  val muted     = "#8d849e"
  val muted2    = "#6f6785"
  val accent    = "#d6409f"
  val accent2   = "#8b5cf6"
  val monoFont  = """ui-monospace, "SF Mono", Menlo, monospace"""
  val sansFont  = """-apple-system, "Segoe UI", system-ui, sans-serif"""

  def sheet: Stylesheet =
    Stylesheet.empty.vars(
      "bg"         -> bg,
      "surface"    -> surface,
      "panel"      -> panel,
      "panel-edge" -> panelEdge,
      "text"       -> text,
      "muted"      -> muted,
      "muted2"     -> muted2,
      "accent"     -> accent,
      "accent2"    -> accent2,
      "mono"       -> monoFont
    )

  /** A token referenced as a CSS variable inside a typed rule. */
  def cssVar(name: String): Color = Color.variable(name)

  /** A validated hex literal; the fallback magenta flags a bad constant loudly instead of silently. */
  def hex(value: String): Color = Color.hex(value).getOrElse(Color.rgb(255, 0, 255))

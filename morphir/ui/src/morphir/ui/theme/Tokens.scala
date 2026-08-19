package morphir.ui.theme

import kyo.*
import kyo.Style.Color

/**
 * The design tokens of the morphir client theme. Every color the shell paints is a CSS custom property, so a color
 * scheme is a set of values rather than a second stylesheet: [[sheet]] emits the dark palette at the root, the light
 * palette scoped to the light class, and the light palette again inside a `prefers-color-scheme: light` query for the
 * hosts that follow the system.
 */
object Tokens:

  /** Class names the shell root carries to select a scheme. */
  object Scheme:
    val system = "theme-system"
    val light  = "theme-light"
    val dark   = "theme-dark"

  val monoFont = """ui-monospace, "SF Mono", Menlo, monospace"""
  val sansFont = """-apple-system, "Segoe UI", system-ui, sans-serif"""

  /** Clearance for the macOS traffic lights, so whatever follows them sits at the same place in every shell state. */
  val trafficLightInset = 78

  /** How long a shell region takes to slide in or out. */
  val slideMs = 320

  /** The palette the shell ships with. */
  val dark: Seq[(String, String)] = Seq(
    "bg"          -> "#0f0d14",
    "surface"     -> "#16131d",
    "panel"       -> "#1a1622",
    "panel-edge"  -> "#2a2438",
    "rail"        -> "#121017",
    "edge"        -> "#241f30",
    "row-edge"    -> "#221d2e",
    "head-edge"   -> "#1d1828",
    "hover"       -> "#1f1a29",
    "hover-soft"  -> "#1a1622",
    "code-bg"     -> "#131019",
    "text"        -> "#e8e4f1",
    "text-strong" -> "#ffffff",
    "muted"       -> "#8d849e",
    "muted2"      -> "#6f6785",
    "nav"         -> "#a89fbe",
    "dot"         -> "#3d3550",
    "accent"      -> "#d6409f",
    "accent2"     -> "#8b5cf6",
    "accent-text" -> "#f2b7dd",
    "knob"        -> "#ffffff",
    "mono"        -> monoFont
  )

  /** The same tokens, lit. Values are chosen so contrast holds where the dark palette relied on glow. */
  val light: Seq[(String, String)] = Seq(
    "bg"          -> "#f6f4fa",
    "surface"     -> "#ffffff",
    "panel"       -> "#ffffff",
    "panel-edge"  -> "#e4dff0",
    "rail"        -> "#f0edf7",
    "edge"        -> "#e0daee",
    "row-edge"    -> "#ebe6f4",
    "head-edge"   -> "#e4dff0",
    "hover"       -> "#eae5f5",
    "hover-soft"  -> "#f0ecf8",
    "code-bg"     -> "#f4f1fa",
    "text"        -> "#1c1726",
    "text-strong" -> "#0f0d14",
    "muted"       -> "#6c6484",
    "muted2"      -> "#847c9c",
    "nav"         -> "#4a4360",
    "dot"         -> "#c9c1de",
    "accent"      -> "#c02e8c",
    "accent2"     -> "#7c4ddb",
    "accent-text" -> "#9c2f77",
    "knob"        -> "#ffffff",
    "mono"        -> monoFont
  )

  def sheet: Stylesheet =
    Stylesheet.empty
      .vars(dark*)
      .scopedVars(Selector.cls(Scheme.dark), dark*)
      .scopedVars(Selector.cls(Scheme.light), light*)
      // Following the system: light unless the OS asks for dark. kyo types `prefers-color-scheme: dark` only, so the
      // light palette is the base and the query puts the dark one back.
      .scopedVars(Selector.cls(Scheme.system), light*)
      .media(Stylesheet.MediaQuery.prefersDark)(
        Stylesheet.empty.scopedVars(Selector.cls(Scheme.system), dark*)
      )

  /** A token referenced as a CSS variable inside a typed rule. */
  def cssVar(name: String): Color = Color.variable(name)

  /** A validated hex literal; the fallback magenta flags a bad constant loudly instead of silently. */
  def hex(value: String): Color = Color.hex(value).getOrElse(Color.rgb(255, 0, 255))

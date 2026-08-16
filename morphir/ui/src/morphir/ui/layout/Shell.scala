package morphir.ui.layout

/** Class names shared across the shell chrome. */
object Shell:
  object Css:
    val app      = "app"
    val body     = "app-body"
    val main     = "main"
    val content  = "content"
    val iconBtn  = "icon-btn"
    val panel    = "panel"
    val settings = "content-settings"
    val noMotion = "no-motion"

  import kyo.*
  import kyo.Style
  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      // The scheme vars are scoped to this element, so it — not the body above it — paints the surface.
      .rule(
        Css.app,
        Style.display(_.flex).column.height(Length.Vh(100)).bg(Tokens.cssVar("bg")).color(Tokens.cssVar("text"))
      )
      .rule(Css.body, Style.display(_.flex).row.flexGrow(1).minWidth(0.px))
      .rule(Css.main, Style.display(_.flex).column.flexGrow(1).minWidth(0.px))
      .rule(
        Css.iconBtn,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .justify(_.center)
          .width(30.px)
          .height(30.px)
          .rounded(8.px)
          .color(Tokens.cssVar("muted"))
          .cursor(_.pointer)
          .hover(_.bg(Tokens.cssVar("hover")).color(Tokens.cssVar("text")))
      )
      .rule(
        Selector.cls(s"${Css.content}.${Css.settings}"),
        Style.padding(28.px, 40.px).maxWidth(980.px)
      )
      .rule(
        Css.panel,
        Style
          .bg(Tokens.cssVar("panel"))
          .border(1.px, Tokens.cssVar("panel-edge"))
          .rounded(12.px)
          .padding(16.px, 18.px, 8.px, 18.px)
          .minWidth(0.px)
      )
      .rule(
        Selector.cls(Css.panel).child(Selector.tag("h2")),
        Style
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
          .fontSize(10.px)
          .fontWeight(_.w600)
          .letterSpacing(0.18.em)
          .textTransform(_.uppercase)
          .color(Tokens.cssVar("muted2"))
          .padding(0.px, 0.px, 12.px, 0.px)
      )

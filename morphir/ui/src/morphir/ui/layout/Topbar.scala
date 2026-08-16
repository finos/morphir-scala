package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.icons.Icons

/**
 * The topbar slot: breadcrumb (plus the sidebar toggle when the sidebar is collapsed) on the left, version chip and the
 * right/bottom region toggles on the right.
 */
object Topbar:

  object Css:
    val root        = "topbar"
    val left        = "topbar-left"
    val right       = "topbar-right"
    val title       = "topbar-title"
    val crumb       = "crumb"
    val chip        = "chip"
    val lightsInset = "lights-inset"

  def view(
      sectionTitle: String,
      version: String,
      state: ShellState,
      customChrome: Boolean,
      leftCollapsed: Boolean
  ): UI =
    val titleGroup =
      div.cssClass(Css.title)(span("morphir / ").cssClass(Css.crumb), sectionTitle)
    val leftGroup =
      if leftCollapsed then
        div.cssClass(Css.left)(
          div.cssClass(Shell.Css.iconBtn).id("sidebar-toggle").onClick(state.left.set(false))(Icons.sidebar),
          titleGroup
        )
      else titleGroup
    val bar =
      if leftCollapsed && customChrome then div.cssClass(Css.root).cssClass(Css.lightsInset)
      else div.cssClass(Css.root)
    bar(
      leftGroup,
      div.cssClass(Css.right)(
        span(s"v$version").cssClass(Css.chip).id("app-version"),
        div.cssClass(Shell.Css.iconBtn).id("bottom-toggle").onClick(state.bottom.getAndUpdate(v => !v))(
          Icons.panelBottom
        ),
        div.cssClass(Shell.Css.iconBtn).id("right-toggle").onClick(state.right.getAndUpdate(v => !v))(
          Icons.panelRight
        )
      )
    )

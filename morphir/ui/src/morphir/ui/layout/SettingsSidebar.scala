package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.AppShell.SettingsSection
import morphir.ui.icons.Icons

/**
 * The settings surface's left column: one row per settings section, and a Back row that returns to the workspace.
 * Shares the sidebar shell (width, color, border) with [[Sidebar]].
 */
object SettingsSidebar:

  object Css:
    val item   = "settings-item"
    val active = "active"
    val back   = "settings-back"
    val label  = "settings-back-label"

  def view(
      sections: Chunk[SettingsSection],
      selected: SettingsKey,
      state: ShellState
  ): UI =
    div.cssClass(Sidebar.Css.root)(
      div.cssClass(Sidebar.Css.navSection)("Settings"),
      fragment(sections.toSeq.map(section => row(section, selected, state))*),
      div.cssClass(Css.back).id("settings-back").onClick(state.closeSettings)(
        Icons.back,
        span("Back").cssClass(Css.label)
      )
    )

  private def row(section: SettingsSection, selected: SettingsKey, state: ShellState): UI =
    val base = div.cssClass(Css.item)
    val elem = if section.key == selected then base.cssClass(Css.active) else base
    elem
      .id(s"settings-section-${section.key.value}")
      .onClick(state.selectSettingsSection(section.key))(section.label)

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Css.item,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .gap(10.px)
          .padding(8.px, 10.px)
          .margin(1.px, 0.px)
          .rounded(8.px)
          .color(Tokens.cssVar("nav"))
          .fontWeight(_.w500)
          .cursor(_.pointer)
          .hover(_.bg(Tokens.cssVar("hover-soft")).color(Tokens.cssVar("text")))
      )
      .rule(
        Selector.cls(s"${Css.item}.${Css.active}"),
        Style.bg(Tokens.cssVar("hover")).color(Tokens.cssVar("text-strong"))
      )
      .rule(
        Css.back,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .gap(10.px)
          .margin(Length.Auto, Length.Px(0), Length.Px(0), Length.Px(0))
          .padding(8.px, 10.px)
          .rounded(8.px)
          .color(Tokens.cssVar("muted"))
          .cursor(_.pointer)
          .hover(_.bg(Tokens.cssVar("hover-soft")).color(Tokens.cssVar("text")))
      )
      .rule(Css.label, Style.fontWeight(_.w500))

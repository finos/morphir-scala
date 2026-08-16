package morphir.ui.theme

import kyo.*
import kyo.test.*
import morphir.ui.{SettingsView, Theme}
import morphir.ui.layout.{RegionPanel, SettingsSidebar, Shell, Sidebar, Topbar}

class ThemeTests extends Test[Any]:

  /** Every class name the layout emits must be styled by the aggregated stylesheet. */
  val layoutClasses = List(
    Shell.Css.app,
    Shell.Css.main,
    Shell.Css.content,
    Shell.Css.iconBtn,
    Shell.Css.panel,
    Sidebar.Css.root,
    Sidebar.Css.navItem,
    Sidebar.Css.foot,
    Shell.Css.body,
    Topbar.Css.root,
    Topbar.Css.brandZone,
    Topbar.Css.rest,
    Topbar.Css.chip,
    RegionPanel.Css.rightbar,
    RegionPanel.Css.bottombar,
    RegionPanel.Css.head,
    SettingsSidebar.Css.item,
    SettingsSidebar.Css.back,
    SettingsView.Css.group,
    SettingsView.Css.row
  )

  "Theme" - {

    "renders tokens as CSS variables" in {
      val css = Theme.css
      assert(css.contains("--accent") && css.contains("--bg") && css.contains(":root"))
    }

    "renders the typed body rule" in
      assert(Theme.css.contains("body"))

    "styles every layout class" in {
      val css     = Theme.css
      val missing = layoutClasses.filterNot(c => css.contains(s".$c"))
      assert(missing.isEmpty)
    }

    "carries a palette for every color scheme" in {
      val css = Theme.css
      assert(
        css.contains(s".${Tokens.Scheme.light}") && css.contains(s".${Tokens.Scheme.dark}") &&
          css.contains(s".${Tokens.Scheme.system}") && css.contains("prefers-color-scheme: dark")
      )
    }

    "keeps the app-region quarantine" in
      assert(Theme.css.contains("-webkit-app-region"))
  }
end ThemeTests

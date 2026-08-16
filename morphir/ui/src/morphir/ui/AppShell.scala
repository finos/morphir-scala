package morphir.ui

import kyo.*
import kyo.UI.*

/**
 * Application chrome shared by every morphir client: left sidebar (brand + navigation), topbar (context + version
 * chip), and a panel grid for content. Styled by [[Theme.css]].
 */
object AppShell:

  final case class NavItem(label: String, active: Boolean = false)

  def panel(title: String, body: UI): UI =
    section.cssClass("panel")(h2(title), body)

  def shell(
      sectionTitle: String,
      version: String,
      nav: Chunk[NavItem],
      panels: Chunk[UI]
  ): UI =
    div.cssClass("app").id("app-root")(
      div.cssClass("sidebar")(
        div.cssClass("brand")(span("morphir").cssClass("brand-mark"), span("DESKTOP").cssClass("brand-sub")),
        div.cssClass("nav-section")("Workspace"),
        fragment(nav.toSeq.map(_.render)*),
        div.cssClass("sidebar-foot")("morphir-scala · kyo-ui")
      ),
      div.cssClass("main")(
        div.cssClass("topbar")(
          div.cssClass("topbar-title")(span("morphir / ").cssClass("crumb"), sectionTitle),
          span(s"v$version").cssClass("chip").id("app-version")
        ),
        div.cssClass("content")(fragment(panels.toSeq*))
      )
    )

  extension (item: NavItem)
    private def render: UI =
      val base = div.cssClass("nav-item")
      val elem = if item.active then base.cssClass("active") else base
      elem(span("").cssClass("nav-dot"), item.label)

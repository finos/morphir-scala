package morphir.ui.internal

import kyo.*
import morphir.ui.layout.{RegionPanel, ShellState, Sidebar, Topbar}
import scala.scalajs.js

/**
 * Writes region sizes onto the live DOM so they animate.
 *
 * A reactive `style` binding cannot animate: kyo-ui rebuilds the element on every emission, so the browser never sees a
 * start value to transition from. Instead this adapter subscribes to the store's extent signals and sets `width` or
 * `height` on the element that is already on screen; the CSS transition declared in the panel's own stylesheet then
 * plays. Collapsing drives the extent to zero rather than unmounting the panel, which is what makes the slide — and the
 * neighbours' reflow — smooth.
 */
private[ui] object PanelMotion:

  def attach(state: ShellState): Unit < Async =
    for
      // The sidebar's own horizontal padding is a floor under border-box sizing, so it collapses with the width.
      _ <- watch(state.leftExtent)(
        px => write(s".${Sidebar.Css.root}", "width", px),
        px => writeGutter(s".${Sidebar.Css.root}", px),
        px => write(s".${Topbar.Css.brandZone}", "width", px)
      )
      _ <- watch(state.rightExtent)(px => write(s".${RegionPanel.Css.rightbar}", "width", px))
      _ <- watch(state.bottomExtent)(px => write(s".${RegionPanel.Css.bottombar}", "height", px))
      // The route swaps whole surfaces, so the elements are new: re-apply the current sizes to them.
      _ <- watchRoute(state)
    yield ()

  private def watch(extent: Signal[Int])(writes: (Int => Unit)*): Unit < Async =
    Fiber.initUnscoped(extent.streamCurrent.foreach(px => Sync.defer(writes.foreach(_(px))))).unit

  private def watchRoute(state: ShellState): Unit < Async =
    Fiber
      .initUnscoped(
        state.route.streamChanges.foreach { _ =>
          for
            visibility <- state.left.get
            leftSize   <- state.leftWidth.get
            rightVis   <- state.right.get
            rightSize  <- state.rightWidth.get
            bottomVis  <- state.bottom.get
            bottomSize <- state.bottomHeight.get
          yield
            val left   = if visibility.isCollapsed then 0 else leftSize.px
            val right  = if rightVis.isCollapsed then 0 else rightSize.px
            val bottom = if bottomVis.isCollapsed then 0 else bottomSize.px
            // A frame later: the new surface's elements are in the document by then.
            js.Dynamic.global.setTimeout(
              () =>
                write(s".${Sidebar.Css.root}", "width", left)
                write(s".${Topbar.Css.brandZone}", "width", left)
                write(s".${RegionPanel.Css.rightbar}", "width", right)
                write(s".${RegionPanel.Css.bottombar}", "height", bottom)
              ,
              0
            )
            ()
        }
      )
      .unit

  /** Zero the horizontal padding while the panel is closed; anything else hands the gutter back to the stylesheet. */
  private def writeGutter(selector: String, px: Int): Unit =
    val element = js.Dynamic.global.document.querySelector(selector)
    if element != null && !js.isUndefined(element) then
      val gutter = if px == 0 then "0" else ""
      val _      = element.style.setProperty("padding-left", gutter)
      val _      = element.style.setProperty("padding-right", gutter)

  private def write(selector: String, property: String, px: Int): Unit =
    val element = js.Dynamic.global.document.querySelector(selector)
    if element != null && !js.isUndefined(element) then
      val _ = element.style.setProperty(property, s"${px}px")

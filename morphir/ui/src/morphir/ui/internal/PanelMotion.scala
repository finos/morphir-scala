package morphir.ui.internal

import kyo.*
import morphir.ui.layout.{RegionPanel, ShellState, Sidebar, Topbar}
import scala.scalajs.js

/**
 * Writes region sizes onto the live DOM so they animate.
 *
 * A reactive `style` binding cannot animate: kyo-ui rebuilds the element on every emission, so the browser never sees a
 * start value to transition from. Instead this adapter subscribes to the store's extent signals and sets `width` or
 * `height` on the element that is already on screen; the transition declared in the panel's own stylesheet then plays.
 * Collapsing drives the extent to zero rather than unmounting the panel, which is what makes the slide — and the
 * neighbours' reflow — smooth.
 *
 * The animation setting is honoured by the shell root's `no-motion` class rather than from here: the root re-renders
 * when the setting changes, and an inline gate written by this adapter would be lost with the element it was written
 * to. The adapter reads the store and writes the DOM; it never owns state.
 */
private[ui] object PanelMotion:

  def attach(state: ShellState): Unit < Async =
    for
      _ <- watch(state.leftExtent)(
        px => write(s".${Sidebar.Css.root}", "width", px),
        // The sidebar's own horizontal padding is a floor under border-box sizing, so it collapses with the width.
        px => writeGutter(s".${Sidebar.Css.root}", px),
        px => write(s".${Topbar.Css.brandZone}", "width", px)
      )
      _ <- watch(state.rightExtent)(px => write(s".${RegionPanel.Css.rightbar}", "width", px))
      _ <- watch(state.bottomExtent)(px => write(s".${RegionPanel.Css.bottombar}", "height", px))
      // Changing route or the animation setting rebuilds the tree, so the fresh elements need the current sizes.
      _ <- reapplyOn(state.route.streamChanges.map(_ => ()), state)
      _ <- reapplyOn(state.animations.streamChanges.map(_ => ()), state)
      // Collapsing the sidebar swaps the titlebar's brand zone out and expanding builds a new one, which starts at
      // the stylesheet's default width. Re-applying a frame later keeps it in step with a dragged sidebar.
      _ <- reapplyOn(state.left.streamChanges.map(_ => ()), state)
    yield ()

  private def watch[A: Tag](signal: Signal[A])(writes: (A => Unit)*)(using CanEqual[A, A]): Unit < Async =
    Fiber.initUnscoped(signal.streamCurrent.foreach(value => Sync.defer(writes.foreach(_(value))))).unit

  private def reapplyOn(changes: Stream[Unit, Async], state: ShellState): Unit < Async =
    Fiber.initUnscoped(changes.foreach(_ => reapply(state))).unit

  private def reapply(state: ShellState): Unit < Sync =
    for
      leftVisible   <- state.left.get
      leftSize      <- state.leftWidth.get
      rightVisible  <- state.right.get
      rightSize     <- state.rightWidth.get
      bottomVisible <- state.bottom.get
      bottomSize    <- state.bottomHeight.get
      motion        <- state.animations.get
    yield
      val left   = if leftVisible.isCollapsed then 0 else leftSize.px
      val right  = if rightVisible.isCollapsed then 0 else rightSize.px
      val bottom = if bottomVisible.isCollapsed then 0 else bottomSize.px
      // A frame later: the new surface's elements are in the document by then.
      val _ = js.Dynamic.global.setTimeout(
        () =>
          write(s".${Sidebar.Css.root}", "width", left)
          writeGutter(s".${Sidebar.Css.root}", left)
          write(s".${Topbar.Css.brandZone}", "width", left)
          write(s".${RegionPanel.Css.rightbar}", "width", right)
          write(s".${RegionPanel.Css.bottombar}", "height", bottom)
        ,
        0
      )
      ()

  /** Zero the horizontal padding while the panel is closed; anything else hands the gutter back to the stylesheet. */
  private def writeGutter(selector: String, px: Int): Unit =
    val gutter = if px == 0 then "0" else ""
    set(selector, "padding-left", gutter)
    set(selector, "padding-right", gutter)

  private def write(selector: String, property: String, px: Int): Unit =
    set(selector, property, s"${px}px")

  private def set(selector: String, property: String, value: String): Unit =
    val element = js.Dynamic.global.document.querySelector(selector)
    if element != null && !js.isUndefined(element) then
      val _ = element.style.setProperty(property, value)

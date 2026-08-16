package morphir.ui.layout

import kyo.*
import kyo.UI.*

/**
 * The thin grab strips between shell regions. The markup and styling live here; the pointer drag itself is DOM work,
 * wired by [[morphir.ui.internal.PointerResize]] against the ids below.
 */
object ResizeHandle:

  object Css:
    val handle     = "resize-handle"
    val vertical   = "resize-vertical"
    val horizontal = "resize-horizontal"

  val leftId   = "left-resize"
  val rightId  = "right-resize"
  val bottomId = "bottom-resize"

  /** A vertical strip: drags a side panel's width. */
  def column(id: String): UI =
    div.cssClass(Css.handle).cssClass(Css.vertical).id(id)

  /** A horizontal strip: drags the bottom panel's height. */
  def row(id: String): UI =
    div.cssClass(Css.handle).cssClass(Css.horizontal).id(id)

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(Css.handle, Style.flexShrink(0))
      .rule(
        Css.vertical,
        Style.width(5.px).hover(_.bg(Style.Color.rgba(214, 64, 159, 0.35)))
      )
      .rule(
        Css.horizontal,
        Style.height(5.px).hover(_.bg(Style.Color.rgba(214, 64, 159, 0.35)))
      )

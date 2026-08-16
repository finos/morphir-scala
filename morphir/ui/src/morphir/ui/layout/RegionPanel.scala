package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.AppShell.Region

/** The right and bottom shell regions: a titled header strip over a scrolling body. */
object RegionPanel:

  object Css:
    val rightbar  = "rightbar"
    val bottombar = "bottombar"
    val head      = "region-head"
    val body      = "region-body"

  def right(region: Region): UI =
    div.cssClass(Css.rightbar)(
      div.cssClass(Css.head)(region.title),
      div.cssClass(Css.body)(region.body)
    )

  def bottom(region: Region): UI =
    div.cssClass(Css.bottombar)(
      div.cssClass(Css.head)(region.title),
      div.cssClass(Css.body)(region.body)
    )

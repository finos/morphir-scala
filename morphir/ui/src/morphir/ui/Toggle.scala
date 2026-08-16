package morphir.ui

import kyo.*
import kyo.UI.*

/**
 * A switch: a pill track with a knob that slides across when it is on. Reads a [[kyo.Signal]] and runs a command on
 * click, so the store stays the only writer.
 */
object Toggle:

  object Css:
    val track = "toggle"
    val on    = "on"
    val knob  = "toggle-knob"

  def view(id: String, on: Signal[Boolean], onToggle: => Any < Async): UI =
    on.render { isOn =>
      val track = div.cssClass(Css.track)
      val elem  = if isOn then track.cssClass(Css.on) else track
      elem.id(id).onClick(onToggle)(div.cssClass(Css.knob))
    }

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Css.track,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .width(44.px)
          .height(24.px)
          .flexShrink(0)
          .padding(4.px)
          .rounded(999.px)
          .bg(Tokens.cssVar("panel-edge"))
          .cursor(_.pointer)
          .transition(_.all, 160, _.easeInOut)
      )
      .rule(
        Selector.cls(s"${Css.track}.${Css.on}"),
        Style.bg(Tokens.cssVar("accent")).justify(_.end)
      )
      .rule(
        Css.knob,
        Style
          .width(16.px)
          .height(16.px)
          .flexShrink(0)
          .rounded(999.px)
          .bg(Tokens.cssVar("knob"))
          .shadow(0.px, 1.px, 2.px, 0.px, Style.Color.rgba(0, 0, 0, 0.45))
          .transition(_.all, 160, _.easeInOut)
      )

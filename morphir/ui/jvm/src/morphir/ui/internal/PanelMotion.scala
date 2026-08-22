package morphir.ui.internal

import kyo.*
import morphir.ui.layout.ShellState

private[ui] object PanelMotion:
  def attach(state: ShellState): Unit < Async =
    val _ = state
    Kyo.unit

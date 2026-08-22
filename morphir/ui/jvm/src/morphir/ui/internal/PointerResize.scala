package morphir.ui.internal

import kyo.*
import morphir.ui.layout.ShellState

private[ui] object PointerResize:
  def attach(state: ShellState): Unit < Sync =
    val _ = state
    Kyo.unit

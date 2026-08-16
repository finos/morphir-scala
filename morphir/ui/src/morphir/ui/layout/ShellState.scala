package morphir.ui.layout

import kyo.*

/**
 * Collapsed flags for the three shell regions. Host-owned [[kyo.SignalRef]]s: the shell stays a pure value and the host
 * decides whether the state is ephemeral or persisted.
 */
final case class ShellState(
    left: SignalRef[Boolean],
    right: SignalRef[Boolean],
    bottom: SignalRef[Boolean]
)

object ShellState:
  def init(
      leftCollapsed: Boolean = false,
      rightCollapsed: Boolean = false,
      bottomCollapsed: Boolean = false
  ): ShellState < Sync =
    for
      left   <- Signal.initRef(leftCollapsed)
      right  <- Signal.initRef(rightCollapsed)
      bottom <- Signal.initRef(bottomCollapsed)
    yield ShellState(left, right, bottom)

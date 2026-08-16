package morphir.ui.layout

import kyo.*

/**
 * Whether a shell region is on screen. An enum rather than a Boolean so call sites read as intent
 * (`left.set(Collapsed)`) and cannot be flipped by accident.
 */
enum RegionVisibility derives CanEqual:
  case Expanded, Collapsed

  def toggled: RegionVisibility = this match
    case Expanded  => Collapsed
    case Collapsed => Expanded

  def isCollapsed: Boolean = this == Collapsed

/**
 * Visibility of the three shell regions. Host-owned [[kyo.SignalRef]]s: the shell stays a pure value and the host
 * decides whether the state is ephemeral or persisted.
 */
final case class ShellState(
    left: SignalRef[RegionVisibility],
    right: SignalRef[RegionVisibility],
    bottom: SignalRef[RegionVisibility]
)

object ShellState:
  def init(
      left: RegionVisibility = RegionVisibility.Expanded,
      right: RegionVisibility = RegionVisibility.Expanded,
      bottom: RegionVisibility = RegionVisibility.Expanded
  ): ShellState < Sync =
    for
      leftRef   <- Signal.initRef(left)
      rightRef  <- Signal.initRef(right)
      bottomRef <- Signal.initRef(bottom)
    yield ShellState(leftRef, rightRef, bottomRef)

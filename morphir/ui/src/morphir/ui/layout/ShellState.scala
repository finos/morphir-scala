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
 * Which top-level surface the shell shows: the workspace, or the settings surface reached from the gear button.
 */
enum ShellRoute derives CanEqual:
  case Workspace, Settings

  def isSettings: Boolean = this == Settings

/** Identifier of a settings section, so the selected section is not a loose String. */
opaque type SettingsKey = String

object SettingsKey:
  def apply(value: String): SettingsKey          = value
  extension (key: SettingsKey) def value: String = key
  given CanEqual[SettingsKey, SettingsKey]       = CanEqual.derived

/**
 * The shell store: region visibility, the active route and the selected settings section, as host-owned
 * [[kyo.SignalRef]]s. Views read the signals and call the commands; they never write a ref themselves.
 */
final case class ShellState(
    left: SignalRef[RegionVisibility],
    right: SignalRef[RegionVisibility],
    bottom: SignalRef[RegionVisibility],
    route: SignalRef[ShellRoute],
    settingsSection: SignalRef[SettingsKey]
):

  /** Leave the workspace for the settings surface, landing on `section`. */
  def openSettings(section: SettingsKey): Unit < Sync =
    settingsSection.set(section).andThen(route.set(ShellRoute.Settings))

  /** Return to the workspace surface. */
  def closeSettings: Unit < Sync = route.set(ShellRoute.Workspace)

  def selectSettingsSection(section: SettingsKey): Unit < Sync = settingsSection.set(section)

object ShellState:
  def init(
      left: RegionVisibility = RegionVisibility.Expanded,
      right: RegionVisibility = RegionVisibility.Expanded,
      bottom: RegionVisibility = RegionVisibility.Expanded,
      route: ShellRoute = ShellRoute.Workspace,
      settingsSection: SettingsKey = SettingsKey("general")
  ): ShellState < Sync =
    for
      leftRef    <- Signal.initRef(left)
      rightRef   <- Signal.initRef(right)
      bottomRef  <- Signal.initRef(bottom)
      routeRef   <- Signal.initRef(route)
      sectionRef <- Signal.initRef(settingsSection)
    yield ShellState(leftRef, rightRef, bottomRef, routeRef, sectionRef)

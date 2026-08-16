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
 * A shell region's size in CSS pixels, kept inside the region's own bounds by its smart constructor so no view can
 * install a degenerate width or height.
 */
opaque type PanelSize = Int

object PanelSize:
  def clamped(px: Int, bounds: PanelBounds): PanelSize = math.max(bounds.min, math.min(bounds.max, px))
  extension (size: PanelSize) def px: Int              = size
  given CanEqual[PanelSize, PanelSize]                 = CanEqual.derived

/** The size range a region may be dragged through. */
final case class PanelBounds(min: Int, max: Int)

object PanelBounds:
  val left   = PanelBounds(180, 420)
  val right  = PanelBounds(220, 560)
  val bottom = PanelBounds(120, 460)

/**
 * The shell store: region visibility, the active route and the selected settings section, as host-owned
 * [[kyo.SignalRef]]s. Views read the signals and call the commands; they never write a ref themselves.
 */
final case class ShellState(
    left: SignalRef[RegionVisibility],
    right: SignalRef[RegionVisibility],
    bottom: SignalRef[RegionVisibility],
    route: SignalRef[ShellRoute],
    settingsSection: SignalRef[SettingsKey],
    leftWidth: SignalRef[PanelSize],
    rightWidth: SignalRef[PanelSize],
    bottomHeight: SignalRef[PanelSize]
):

  /** Leave the workspace for the settings surface, landing on `section`. */
  def openSettings(section: SettingsKey): Unit < Sync =
    settingsSection.set(section).andThen(route.set(ShellRoute.Settings))

  /** Return to the workspace surface. */
  def closeSettings: Unit < Sync = route.set(ShellRoute.Workspace)

  def selectSettingsSection(section: SettingsKey): Unit < Sync = settingsSection.set(section)

  /** Resize commands. Each clamps into its region's bounds, so a drag that runs past the edge simply stops. */
  def resizeLeft(px: Int): Unit < Sync   = leftWidth.set(PanelSize.clamped(px, PanelBounds.left))
  def resizeRight(px: Int): Unit < Sync  = rightWidth.set(PanelSize.clamped(px, PanelBounds.right))
  def resizeBottom(px: Int): Unit < Sync = bottomHeight.set(PanelSize.clamped(px, PanelBounds.bottom))

object ShellState:
  def init(
      left: RegionVisibility = RegionVisibility.Expanded,
      right: RegionVisibility = RegionVisibility.Expanded,
      bottom: RegionVisibility = RegionVisibility.Expanded,
      route: ShellRoute = ShellRoute.Workspace,
      settingsSection: SettingsKey = SettingsKey("general"),
      leftWidth: Int = 224,
      rightWidth: Int = 300,
      bottomHeight: Int = 180
  ): ShellState < Sync =
    for
      leftRef    <- Signal.initRef(left)
      rightRef   <- Signal.initRef(right)
      bottomRef  <- Signal.initRef(bottom)
      routeRef   <- Signal.initRef(route)
      sectionRef <- Signal.initRef(settingsSection)
      leftSize   <- Signal.initRef(PanelSize.clamped(leftWidth, PanelBounds.left))
      rightSize  <- Signal.initRef(PanelSize.clamped(rightWidth, PanelBounds.right))
      bottomSize <- Signal.initRef(PanelSize.clamped(bottomHeight, PanelBounds.bottom))
    yield ShellState(leftRef, rightRef, bottomRef, routeRef, sectionRef, leftSize, rightSize, bottomSize)

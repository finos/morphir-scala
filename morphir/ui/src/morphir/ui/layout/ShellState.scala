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
 * Whether the shell animates region transitions. Off is a first-class setting, not a missing Boolean: reduced-motion
 * users and screenshot tests both want it.
 */
enum AnimationSetting derives CanEqual:
  case Enabled, Disabled

  def toggled: AnimationSetting = this match
    case Enabled  => Disabled
    case Disabled => Enabled

  def isEnabled: Boolean = this == Enabled

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
    bottomHeight: SignalRef[PanelSize],
    animations: SignalRef[AnimationSetting]
):

  /** Leave the workspace for the settings surface, landing on `section`. */
  def openSettings(section: SettingsKey): Unit < Sync =
    settingsSection.set(section).andThen(route.set(ShellRoute.Settings))

  /** Return to the workspace surface. */
  def closeSettings: Unit < Sync = route.set(ShellRoute.Workspace)

  def selectSettingsSection(section: SettingsKey): Unit < Sync = settingsSection.set(section)

  /**
   * The width the left column should render at: its dragged size, or zero while it is collapsed. Collapsing changes
   * this signal rather than unmounting the panel, so the CSS transition can play and the neighbours reflow with it.
   */
  def leftExtent(using Frame): Signal[Int] =
    left.combineLatest(leftWidth).map((visibility, size) => if visibility.isCollapsed then 0 else size.px)

  def rightExtent(using Frame): Signal[Int] =
    right.combineLatest(rightWidth).map((visibility, size) => if visibility.isCollapsed then 0 else size.px)

  def bottomExtent(using Frame): Signal[Int] =
    bottom.combineLatest(bottomHeight).map((visibility, size) => if visibility.isCollapsed then 0 else size.px)

  /** Turn region animation on or off. */
  def toggleAnimations: Unit < Sync = animations.getAndUpdate(_.toggled).unit

  /** Put the shell back the way it ships: every region open at its default size, animations on. */
  def restoreDefaults: Unit < Sync =
    for
      _ <- left.set(RegionVisibility.Expanded)
      _ <- right.set(RegionVisibility.Expanded)
      _ <- bottom.set(RegionVisibility.Expanded)
      _ <- leftWidth.set(PanelSize.clamped(ShellDefaults.leftWidth, PanelBounds.left))
      _ <- rightWidth.set(PanelSize.clamped(ShellDefaults.rightWidth, PanelBounds.right))
      _ <- bottomHeight.set(PanelSize.clamped(ShellDefaults.bottomHeight, PanelBounds.bottom))
      _ <- animations.set(AnimationSetting.Enabled)
    yield ()

  /** Resize commands. Each clamps into its region's bounds, so a drag that runs past the edge simply stops. */
  def resizeLeft(px: Int): Unit < Sync   = leftWidth.set(PanelSize.clamped(px, PanelBounds.left))
  def resizeRight(px: Int): Unit < Sync  = rightWidth.set(PanelSize.clamped(px, PanelBounds.right))
  def resizeBottom(px: Int): Unit < Sync = bottomHeight.set(PanelSize.clamped(px, PanelBounds.bottom))

/** The sizes and settings the shell ships with, shared by [[ShellState.init]] and `restoreDefaults`. */
object ShellDefaults:
  val leftWidth    = 224
  val rightWidth   = 300
  val bottomHeight = 180

object ShellState:
  def init(
      left: RegionVisibility = RegionVisibility.Expanded,
      right: RegionVisibility = RegionVisibility.Expanded,
      bottom: RegionVisibility = RegionVisibility.Expanded,
      route: ShellRoute = ShellRoute.Workspace,
      settingsSection: SettingsKey = SettingsKey("general"),
      leftWidth: Int = ShellDefaults.leftWidth,
      rightWidth: Int = ShellDefaults.rightWidth,
      bottomHeight: Int = ShellDefaults.bottomHeight,
      animations: AnimationSetting = AnimationSetting.Enabled
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
      motionRef  <- Signal.initRef(animations)
    yield ShellState(leftRef, rightRef, bottomRef, routeRef, sectionRef, leftSize, rightSize, bottomSize, motionRef)

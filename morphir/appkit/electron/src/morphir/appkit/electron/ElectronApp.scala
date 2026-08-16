package morphir.appkit.electron

import kyo.*
import morphir.appkit.electron.internal.facades
import scala.scalajs.js

/**
 * Minimal public surface over the Electron main-process lifecycle. Hosts call [[whenReady]] from their entry point,
 * create a [[Window]], and hand it to [[ElectronPorts.mainPort]]. Everything Electron-native stays behind this object;
 * no facade type leaks.
 */
object ElectronApp:

  final class Window private[electron] (private[electron] val underlying: facades.BrowserWindow):
    def loadFile(path: String): Unit =
      val _ = underlying.loadFile(path)

  /**
   * How the window frame is drawn. [[Chrome.Native]] keeps the platform title bar. [[Chrome.Custom]] hides it so the
   * app draws its own chrome; on macOS the traffic lights stay as a native overlay at the given inset, and the renderer
   * must mark drag regions (`-webkit-app-region`) itself.
   */
  enum Chrome derives CanEqual:
    case Native
    case Custom(trafficLightX: Int = 14, trafficLightY: Int = 14)

  final case class WindowOptions(
      width: Int = 1100,
      height: Int = 780,
      show: Boolean = true,
      preloadPath: Maybe[String] = Absent,
      chrome: Chrome = Chrome.Native,
      backgroundColor: Maybe[String] = Absent
  )

  def whenReady(run: => Unit): Unit =
    val _ = facades.app.whenReady().`then`[Unit] { (_: Unit) =>
      run
    }

  def createWindow(options: WindowOptions): Window =
    val webPreferences = js.Dynamic.literal()
    options.preloadPath match
      case Present(path) => webPreferences.preload = path
      case Absent        => ()
    val windowOptions = js.Dynamic.literal(
      width = options.width,
      height = options.height,
      show = options.show,
      webPreferences = webPreferences
    )
    options.backgroundColor match
      case Present(color) => windowOptions.backgroundColor = color
      case Absent         => ()
    options.chrome match
      case Chrome.Custom(x, y) =>
        windowOptions.titleBarStyle = "hiddenInset"
        windowOptions.trafficLightPosition = js.Dynamic.literal(x = x, y = y)
      case Chrome.Native => ()
    val browserWindow = new facades.BrowserWindow(windowOptions.asInstanceOf[js.Object])
    Window(browserWindow)

  def appVersion: String = facades.app.getVersion()

  def quit(): Unit = facades.app.quit()

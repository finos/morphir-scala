package morphir.appkit.electron

import kyo.*
import morphir.appkit.electron.internal.facades
import scala.scalajs.js

/** The window-global contract a hand-written preload exposes via contextBridge. */
trait IpcBridge:
  def postMessage(message: String): Unit
  def onMessage(handler: js.Function1[String, Unit]): Unit

object ElectronPorts:

  /**
   * Renderer-side port over the preload bridge. Bridge callbacks are plain JS functions, so inbound messages enter
   * through the channel's unsafe offer; a full or closed channel drops the message rather than throwing into the JS
   * callback.
   */
  def rendererPort(bridge: IpcBridge, capacity: Int = 64): IpcPort < Sync =
    Channel.initUnscoped[String](capacity).map { in =>
      bridge.onMessage { (message: String) =>
        import AllowUnsafe.embrace.danger
        val _ = in.unsafe.offer(message)
      }
      channelBacked(in, message => bridge.postMessage(message))
    }

  /**
   * Main-process port for one window over ipcMain + webContents. Facade glue; exercised by the desktop shell's Electron
   * smoke test, not unit tests.
   */
  def mainPort(web: facades.WebContents, channel: String = "morphir-rpc", capacity: Int = 64): IpcPort < Sync =
    Channel.initUnscoped[String](capacity).map { in =>
      facades.ipcMain.on(
        channel,
        (_, message) =>
          import AllowUnsafe.embrace.danger
          val _ = in.unsafe.offer(message)
      )
      channelBacked(in, message => web.send(channel, message))
    }

  private def channelBacked(in: Channel[String], transmit: String => Unit): IpcPort =
    new IpcPort:
      def send(message: String): Unit < (Async & Abort[Closed]) = Sync.defer(transmit(message))
      def incoming: Stream[String, Async & Abort[Closed]]       = in.stream()
      def close: Unit < Async                                   = in.close.unit

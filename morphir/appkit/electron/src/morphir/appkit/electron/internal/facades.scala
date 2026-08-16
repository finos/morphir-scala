package morphir.appkit.electron.internal

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/**
 * Minimal `js.native` declarations over the Electron main-process APIs this leaf touches. Glue only — nothing here is
 * unit-testable without a live Electron; behavior above these seams is tested with fakes.
 */
private[electron] object facades:

  /** Node Buffer, as returned by `safeStorage.encryptString`. */
  @js.native
  trait Buffer extends js.typedarray.Uint8Array

  @js.native
  @JSImport("buffer", "Buffer")
  object BufferModule extends js.Object:
    def from(array: js.typedarray.Uint8Array): Buffer = js.native

  def toBuffer(bytes: Array[Byte]): Buffer =
    val arr = new js.typedarray.Uint8Array(bytes.length)
    var i   = 0
    while i < bytes.length do
      arr(i) = (bytes(i) & 0xff).toShort
      i += 1
    BufferModule.from(arr)

  @js.native
  trait WebContents extends js.Object:
    def send(channel: String, message: String): Unit = js.native

  @js.native
  trait BrowserWindowInstance extends js.Object:
    def loadURL(url: String): js.Promise[Unit]   = js.native
    def loadFile(path: String): js.Promise[Unit] = js.native
    def webContents: WebContents                 = js.native

  @js.native
  @JSImport("electron", "BrowserWindow")
  class BrowserWindow(options: js.Object) extends js.Object:
    def loadURL(url: String): js.Promise[Unit]   = js.native
    def loadFile(path: String): js.Promise[Unit] = js.native
    def webContents: WebContents                 = js.native

  @js.native
  @JSImport("electron", "app")
  object app extends js.Object:
    def whenReady(): js.Promise[Unit]                   = js.native
    def getPath(name: String): String                   = js.native
    def getVersion(): String                            = js.native
    def quit(): Unit                                    = js.native
    def on(event: String, cb: js.Function0[Unit]): Unit = js.native

  @js.native
  @JSImport("electron", "ipcMain")
  object ipcMain extends js.Object:
    def on(channel: String, listener: js.Function2[js.Object, String, Unit]): Unit = js.native

  @js.native
  @JSImport("electron", "dialog")
  object dialog extends js.Object:
    def showOpenDialog(options: js.Object): js.Promise[js.Dynamic] = js.native

  @js.native
  @JSImport("electron", "safeStorage")
  object safeStorage extends js.Object:
    def isEncryptionAvailable(): Boolean         = js.native
    def encryptString(plain: String): Buffer     = js.native
    def decryptString(encrypted: Buffer): String = js.native

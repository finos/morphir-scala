package morphir.appkit.electron.internal

import kyo.*
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.{Failure, Success}

/**
 * Minimal `js.native` declarations over the Electron main-process APIs this leaf touches. Glue only — nothing here is
 * unit-testable without a live Electron; behavior above these seams is tested with fakes.
 */
private[electron] object facades:

  /** Node Buffer, as returned by `safeStorage.encryptStringAsync`. */
  @js.native
  trait Buffer extends js.typedarray.Uint8Array

  @js.native
  @JSImport("buffer", "Buffer")
  object BufferModule extends js.Object:
    def from(array: js.typedarray.Uint8Array): Buffer = js.native

  def toBuffer(bytes: Array[Byte]): Buffer =
    BufferModule.from(toUint8Array(bytes))

  def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val arr = new Uint8Array(bytes.length)
    var i   = 0
    while i < bytes.length do
      arr(i) = (bytes(i) & 0xff).toShort
      i += 1
    arr

  def toSpan(bytes: Uint8Array): Span[Byte] =
    val arr = new Array[Byte](bytes.length)
    var i   = 0
    while i < bytes.length do
      arr(i) = bytes(i).toByte
      i += 1
    Span.from(arr)

  def awaitPromise[A](promise: => js.Promise[A]): Result[Throwable, A] < Async =
    try
      given ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
      val completed          = promise.toFuture.transform {
        case Success(value) => Success(Result.succeed(value))
        case Failure(error) => Success(Result.fail(error))
      }
      Async.fromFuture(completed)
    catch case error: Throwable => Result.fail(error)

  def isLinux: Boolean =
    js.Dynamic.global.process.platform.asInstanceOf[String] == "linux"

  val recursiveDirectoryOptions: js.Object = js.Dynamic.literal(recursive = true)

  @js.native
  @JSImport("node:fs", JSImport.Namespace)
  object nodeFs extends js.Object:
    def existsSync(path: String): Boolean                   = js.native
    def readFileSync(path: String): Uint8Array              = js.native
    def mkdirSync(path: String, options: js.Object): Unit   = js.native
    def openSync(path: String, flags: String): Int          = js.native
    def writeFileSync(handle: Int, bytes: Uint8Array): Unit = js.native
    def fsyncSync(handle: Int): Unit                        = js.native
    def closeSync(handle: Int): Unit                        = js.native
    def renameSync(from: String, to: String): Unit          = js.native
    def unlinkSync(path: String): Unit                      = js.native

  @js.native
  @JSImport("node:path", JSImport.Namespace)
  object nodePath extends js.Object:
    def dirname(path: String): String = js.native

  @js.native
  @JSImport("node:crypto", JSImport.Namespace)
  object nodeCrypto extends js.Object:
    def randomUUID(): String = js.native

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
    def isAsyncEncryptionAvailable(): js.Promise[Boolean]                = js.native
    def encryptStringAsync(plain: String): js.Promise[Buffer]            = js.native
    def decryptStringAsync(encrypted: Buffer): js.Promise[DecryptResult] = js.native
    def getSelectedStorageBackend(): String                              = js.native

  @js.native
  trait DecryptResult extends js.Object:
    def result: String           = js.native
    def shouldReEncrypt: Boolean = js.native

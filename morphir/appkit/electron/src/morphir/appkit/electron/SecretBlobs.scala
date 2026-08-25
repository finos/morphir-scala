package morphir.appkit.electron

import kyo.*
import morphir.appkit.*
import morphir.appkit.electron.internal.facades

/** Persists encrypted secret blobs under stable service and account keys. */
trait SecretBlobs:
  def read(service: String, account: String): Maybe[Span[Byte]] < (Async & Abort[SecretException])
  def writeAtomically(
      service: String,
      account: String,
      bytes: Span[Byte]
  ): Unit < (Async & Abort[SecretException])
  def delete(service: String, account: String): Unit < (Async & Abort[SecretException])

object SecretBlobs:

  private[electron] trait FileSystem:
    def read(path: String): Maybe[Span[Byte]]
    def siblingTemporary(path: String): String
    def openForWrite(path: String): Int
    def write(handle: Int, bytes: Span[Byte]): Unit
    def sync(handle: Int): Unit
    def close(handle: Int): Unit
    def replace(from: String, to: String): Unit
    def delete(path: String): Unit

  private[electron] def fileSystem(directory: String, fs: FileSystem): SecretBlobs =
    FileSystemBlobs(directory, fs)

  private[electron] def electron(directory: String): SecretBlobs =
    fileSystem(directory, ElectronFileSystem)

  private final class FileSystemBlobs(directory: String, fs: FileSystem) extends SecretBlobs:
    def read(service: String, account: String): Maybe[Span[Byte]] < (Async & Abort[SecretException]) =
      Abort.catching[Throwable](_ => SecretException.LookupFailed("Electron secret blob lookup failed"))(
        Async.defer(fs.read(target(service, account)))
      )

    def writeAtomically(
        service: String,
        account: String,
        bytes: Span[Byte]
    ): Unit < (Async & Abort[SecretException]) =
      Abort.catching[Throwable](_ => SecretException.MutationFailed("write"))(
        Async.defer(write(target(service, account), bytes))
      )

    def delete(service: String, account: String): Unit < (Async & Abort[SecretException]) =
      Abort.catching[Throwable](_ => SecretException.MutationFailed("delete"))(
        Async.defer(fs.delete(target(service, account)))
      )

    private def write(targetPath: String, bytes: Span[Byte]): Unit =
      val temporary = fs.siblingTemporary(targetPath)
      var handle    = Absent: Maybe[Int]
      var created   = false
      var installed = false
      try
        val opened = fs.openForWrite(temporary)
        created = true
        handle = Present(opened)
        fs.write(opened, bytes)
        fs.sync(opened)
        fs.close(opened)
        handle = Absent
        fs.replace(temporary, targetPath)
        installed = true
      catch
        case failure: Throwable =>
          handle match
            case Present(opened) => discardFailure(fs.close(opened))
            case Absent          => ()
          throw failure
      finally
        if created && !installed then discardFailure(fs.delete(temporary))

    private def target(service: String, account: String): String =
      s"$directory/${encode(service)}-${encode(account)}.secret"

    private def encode(value: String): String =
      val digits = "0123456789abcdef"
      value.getBytes("UTF-8").iterator
        .flatMap { byte =>
          val unsigned = byte & 0xff
          Iterator(digits.charAt(unsigned >>> 4), digits.charAt(unsigned & 0x0f))
        }
        .mkString

    private def discardFailure(action: => Unit): Unit =
      try action
      catch case _: Throwable => ()

  private object ElectronFileSystem extends FileSystem:
    def read(path: String): Maybe[Span[Byte]] =
      if facades.nodeFs.existsSync(path) then Present(facades.toSpan(facades.nodeFs.readFileSync(path)))
      else Absent

    def siblingTemporary(path: String): String =
      s"$path.${facades.nodeCrypto.randomUUID()}.tmp"

    def openForWrite(path: String): Int =
      facades.nodeFs.mkdirSync(facades.nodePath.dirname(path), facades.recursiveDirectoryOptions)
      facades.nodeFs.openSync(path, "wx")

    def write(handle: Int, bytes: Span[Byte]): Unit =
      facades.nodeFs.writeFileSync(handle, facades.toUint8Array(bytes.toArray))

    def sync(handle: Int): Unit = facades.nodeFs.fsyncSync(handle)

    def close(handle: Int): Unit = facades.nodeFs.closeSync(handle)

    def replace(from: String, to: String): Unit = facades.nodeFs.renameSync(from, to)

    def delete(path: String): Unit =
      if facades.nodeFs.existsSync(path) then facades.nodeFs.unlinkSync(path)

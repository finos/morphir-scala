package morphir.appkit.electron

import kyo.*
import morphir.appkit.*
import morphir.appkit.electron.internal.facades

/** Decrypts stored secret blobs. The Electron implementation wraps safeStorage.decryptString. */
trait SecretCipher:
  def decrypt(bytes: Chunk[Byte]): String < (Abort[SecretException] & Async)

/** Reads persisted encrypted blobs keyed by (service, account). */
trait SecretBlobs:
  def read(service: String, account: String): Maybe[Chunk[Byte]] < (Abort[SecretException] & Async)

object ElectronSecretStore:

  def apply(cipher: SecretCipher, blobs: SecretBlobs): SecretStore =
    new SecretStore:
      def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
        blobs.read(service, account).map {
          case Present(bytes) => cipher.decrypt(bytes).map(Secret.fromStored)
          case Absent         => Absent
        }

  /**
   * SecretStore over Electron safeStorage. Facade glue on the cipher side; blob persistence is host-provided (the
   * desktop shell owns app paths).
   */
  def safeStorage(blobs: SecretBlobs): SecretStore =
    apply(safeStorageCipher, blobs)

  private def safeStorageCipher: SecretCipher =
    new SecretCipher:
      def decrypt(bytes: Chunk[Byte]): String < (Abort[SecretException] & Async) =
        // safeStorage throws when the OS key has changed, encryption is unavailable, or the blob is corrupt.
        // Those are ordinary lookup failures, so they belong in the declared channel rather than in a panic.
        Abort.catching[Throwable](failure => SecretException.LookupFailed(describe(failure)))(
          Sync.defer(facades.safeStorage.decryptString(facades.toBuffer(bytes.toArray)))
        )

  private def describe(failure: Throwable): String =
    val detail = Option(failure.getMessage).filter(_.nonEmpty).getOrElse(failure.toString)
    s"safeStorage could not decrypt the stored secret: $detail"

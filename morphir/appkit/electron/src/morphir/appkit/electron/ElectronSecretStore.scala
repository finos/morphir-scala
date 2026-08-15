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
        Sync.defer(facades.safeStorage.decryptString(facades.toBuffer(bytes.toArray)))

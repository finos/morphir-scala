package morphir.appkit.electron

import kyo.*
import morphir.appkit.*
import morphir.appkit.electron.internal.facades

/** Writable Electron secret storage backed by asynchronous safeStorage encryption and atomic ciphertext files. */
object ElectronSecretVault:

  /** Builds the device vault after Electron is ready. Absent means remembering credentials is unavailable. */
  def system: Maybe[SecretVault] < Async =
    Async.defer {
      val directory = s"${facades.app.getPath("userData")}/secrets"
      available(
        SecretCipher.electron,
        SecretBlobs.electron(directory),
        facades.isLinux,
        () => facades.safeStorage.getSelectedStorageBackend()
      )
    }

  private[electron] def available(
      cipher: SecretCipher,
      blobs: SecretBlobs,
      isLinux: Boolean,
      selectedBackend: () => String
  ): Maybe[SecretVault] < Async =
    cipher.available().map { encryptionAvailable =>
      if !encryptionAvailable || (isLinux && weakBackend(selectedBackend)) then Absent
      else Present(Vault(cipher, blobs))
    }

  private def weakBackend(selectedBackend: () => String): Boolean =
    try
      selectedBackend() match
        case "basic_text" | "unknown" => true
        case _                        => false
    catch case _: Throwable => true

  private final class Vault(cipher: SecretCipher, blobs: SecretBlobs) extends SecretVault:
    def get(service: String, account: String): Maybe[Secret] < (Async & Abort[SecretException]) =
      blobs.read(service, account).map {
        case Present(bytes) =>
          cipher.decryptForVault(bytes).map { decrypted =>
            decrypted.replacement match
              case Present(replacement) =>
                blobs.writeAtomically(service, account, replacement).andThen(Present(decrypted.secret))
              case Absent => Present(decrypted.secret)
          }
        case Absent => Absent
      }

    def put(service: String, account: String, secret: Secret): Unit < (Async & Abort[SecretException]) =
      cipher.available().map { isAvailable =>
        if isAvailable then cipher.encrypt(secret).map(blobs.writeAtomically(service, account, _))
        else Abort.fail(SecretException.NotAvailable("Electron secure storage is unavailable"))
      }

    def remove(service: String, account: String): Unit < (Async & Abort[SecretException]) =
      blobs.delete(service, account)

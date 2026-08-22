package morphir.appkit.electron

import kyo.*
import morphir.appkit.*
import morphir.appkit.electron.internal.facades

/** Encrypts and decrypts secrets without exposing an Electron facade type. */
trait SecretCipher:
  def available(): Boolean < Async
  def encrypt(secret: Secret): Span[Byte] < (Async & Abort[SecretException])
  def decrypt(bytes: Span[Byte]): Secret < (Async & Abort[SecretException])

  private[electron] def decryptForVault(
      bytes: Span[Byte]
  ): SecretCipher.Decryption < (Async & Abort[SecretException]) =
    decrypt(bytes).map(SecretCipher.Decryption(_, Absent))

object SecretCipher:

  private[electron] final case class Decryption(secret: Secret, replacement: Maybe[Span[Byte]])
  private[electron] final case class DecryptedString(result: String, shouldReEncrypt: Boolean)

  private[electron] trait SafeStorageApi:
    def asyncEncryptionAvailable(): Result[Throwable, Boolean] < Async
    def encryptString(value: String): Result[Throwable, Span[Byte]] < Async
    def decryptString(bytes: Span[Byte]): Result[Throwable, DecryptedString] < Async

  private[electron] def safeStorage(api: SafeStorageApi): SecretCipher =
    SafeStorageCipher(api)

  private[electron] val electron: SecretCipher =
    safeStorage(ElectronSafeStorage)

  private final class SafeStorageCipher(api: SafeStorageApi) extends SecretCipher:
    def available(): Boolean < Async =
      api.asyncEncryptionAvailable().map {
        case Result.Success(value) => value
        case Result.Failure(_)     => false
        case Result.Panic(_)       => false
      }

    def encrypt(secret: Secret): Span[Byte] < (Async & Abort[SecretException]) =
      api.encryptString(secret.unsafeReveal).map {
        case Result.Success(bytes) => bytes
        case Result.Failure(_)     => Abort.fail(SecretException.MutationFailed("encrypt"))
        case Result.Panic(_)       => Abort.fail(SecretException.MutationFailed("encrypt"))
      }

    def decrypt(bytes: Span[Byte]): Secret < (Async & Abort[SecretException]) =
      decryptForVault(bytes).map(_.secret)

    override private[electron] def decryptForVault(
        bytes: Span[Byte]
    ): Decryption < (Async & Abort[SecretException]) =
      api.decryptString(bytes).map {
        case Result.Success(value) =>
          Secret.fromStored(value.result) match
            case Present(secret) if value.shouldReEncrypt =>
              encrypt(secret).map(replacement => Decryption(secret, Present(replacement)))
            case Present(secret) => Decryption(secret, Absent)
            case Absent          => Abort.fail(lookupFailure)
        case Result.Failure(_) => Abort.fail(lookupFailure)
        case Result.Panic(_)   => Abort.fail(lookupFailure)
      }

  private object ElectronSafeStorage extends SafeStorageApi:
    def asyncEncryptionAvailable(): Result[Throwable, Boolean] < Async =
      facades.awaitPromise(facades.safeStorage.isAsyncEncryptionAvailable())

    def encryptString(value: String): Result[Throwable, Span[Byte]] < Async =
      facades.awaitPromise(facades.safeStorage.encryptStringAsync(value)).map {
        case Result.Success(buffer) => Result.succeed(facades.toSpan(buffer))
        case Result.Failure(error)  => Result.fail(error)
        case Result.Panic(error)    => Result.panic(error)
      }

    def decryptString(bytes: Span[Byte]): Result[Throwable, DecryptedString] < Async =
      facades.awaitPromise(facades.safeStorage.decryptStringAsync(facades.toBuffer(bytes.toArray))).map {
        case Result.Success(value) => Result.succeed(DecryptedString(value.result, value.shouldReEncrypt))
        case Result.Failure(error) => Result.fail(error)
        case Result.Panic(error)   => Result.panic(error)
      }

  private def lookupFailure: SecretException =
    SecretException.LookupFailed("Electron safeStorage could not decrypt the stored secret")

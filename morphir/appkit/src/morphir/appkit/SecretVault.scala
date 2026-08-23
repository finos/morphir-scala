package morphir.appkit

import kyo.*
import morphir.appkit.internal.KeyringAccess

/** Reads, writes, and removes secrets in a host password store. */
trait SecretVault extends SecretStore:
  def put(service: String, account: String, secret: Secret): Unit < (Abort[SecretException] & Async)
  def remove(service: String, account: String): Unit < (Abort[SecretException] & Async)

object SecretVault:

  def system: SecretVault =
    SystemVault(KeyringAccess.platform)

  private[appkit] def system(keyring: KeyringAccess): SecretVault =
    SystemVault(keyring)

  private final class SystemVault(keyring: KeyringAccess) extends SecretVault:
    def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
      lookup(keyring.get(service, account)).map(Secret.fromStored)

    def put(service: String, account: String, secret: Secret): Unit < (Abort[SecretException] & Async) =
      mutate("set")(keyring.set(service, account, secret.unsafeReveal))

    def remove(service: String, account: String): Unit < (Abort[SecretException] & Async) =
      mutate("delete")(keyring.delete(service, account))

    private def lookup[A](effect: A < Async): A < (Abort[SecretException] & Async) =
      Abort.catching[Throwable] {
        case error: SecretException.NotAvailable => error
        case _                                   => SecretException.LookupFailed("System keyring lookup failed")
      }(effect)

    private def mutate[A](operation: String)(effect: A < Async): A < (Abort[SecretException] & Async) =
      Abort.catching[Throwable] {
        case error: SecretException.NotAvailable => error
        case _                                   => SecretException.MutationFailed(operation)
      }(effect)

package morphir.appkit

import kyo.*
import morphir.appkit.internal.KeyringAccess
import morphir.appkit.internal.SecurityCli

/** Reads a secret from an OS password store. A missing entry is Absent, not an error. */
trait SecretStore:
  def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async)

object SecretStore:

  def const(entries: (String, String, String)*): SecretStore =
    ConstStore(entries.map { case (service, account, secret) => (service, account) -> secret }.toMap)

  def constLayer(entries: (String, String, String)*): Layer[SecretStore, Any] =
    Layer(const(entries*))

  def javaKeychain: SecretStore =
    SecretVault.system

  def javaKeychainLayer: Layer[SecretStore, Any] =
    Layer(javaKeychain)

  def macOsKeychain: SecretStore =
    MacOsKeychainStore(SecurityCli.platform)

  def macOsKeychainLayer: Layer[SecretStore, Any] =
    Layer(macOsKeychain)

  private[appkit] def javaKeychain(keyring: KeyringAccess): SecretStore =
    SecretVault.system(keyring)

  private[appkit] def macOsKeychain(security: SecurityCli): SecretStore =
    MacOsKeychainStore(security)

  private final class ConstStore(entries: Map[(String, String), String]) extends SecretStore:
    def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
      entries.get((service, account)) match
        case Some(secret) => Secret.fromStored(secret)
        case None         => Absent

  private final class MacOsKeychainStore(security: SecurityCli) extends SecretStore:
    def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
      security.findGenericPassword(service, account).map {
        case Present(raw) => Secret.fromStored(raw)
        case Absent       => Absent
      }

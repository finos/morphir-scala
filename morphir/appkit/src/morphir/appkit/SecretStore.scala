package morphir.appkit

import kyo.*
import morphir.appkit.internal.KeyringGet
import morphir.appkit.internal.SecurityCli

/** Reads a secret from an OS password store. A missing entry is Absent, not an error. */
trait SecretStore:
  def get(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async)

object SecretStore:

  def const(entries: (String, String, String)*): SecretStore =
    ConstStore(entries.map { case (service, account, secret) => (service, account) -> secret }.toMap)

  def constLayer(entries: (String, String, String)*): Layer[SecretStore, Any] =
    Layer(const(entries*))

  def javaKeychain: SecretStore =
    JavaKeychainStore(KeyringGet.platform)

  def javaKeychainLayer: Layer[SecretStore, Any] =
    Layer(javaKeychain)

  def macOsKeychain: SecretStore =
    MacOsKeychainStore(SecurityCli.platform)

  def macOsKeychainLayer: Layer[SecretStore, Any] =
    Layer(macOsKeychain)

  private[appkit] def javaKeychain(keyring: KeyringGet): SecretStore =
    JavaKeychainStore(keyring)

  private[appkit] def macOsKeychain(security: SecurityCli): SecretStore =
    MacOsKeychainStore(security)

  private final class ConstStore(entries: Map[(String, String), String]) extends SecretStore:
    def get(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
      entries.get((service, account)) match
        case Some(secret) => Present(secret)
        case None         => Absent

  private final class JavaKeychainStore(keyring: KeyringGet) extends SecretStore:
    def get(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
      keyring.password(service, account)

  private final class MacOsKeychainStore(security: SecurityCli) extends SecretStore:
    def get(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
      security.findGenericPassword(service, account)

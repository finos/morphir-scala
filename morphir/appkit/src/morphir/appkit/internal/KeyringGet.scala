package morphir.appkit
package internal

import kyo.*

/** Reads one password from a Java keyring. Tests inject a fake. */
private[appkit] trait KeyringGet:
  def password(service: String, account: String): Maybe[String] < (Abort[SecretException] & Async)

private[appkit] object KeyringGet:
  def succeed(secret: String): KeyringGet =
    new KeyringGet:
      def password(service: String, account: String) =
        val _ = (service, account)
        Present(secret)

  def missing: KeyringGet =
    new KeyringGet:
      def password(service: String, account: String) =
        val _ = (service, account)
        Absent

  def fail(error: SecretException): KeyringGet =
    new KeyringGet:
      def password(service: String, account: String) =
        val _ = (service, account)
        Abort.fail(error)

  def platform: KeyringGet = PlatformKeyring

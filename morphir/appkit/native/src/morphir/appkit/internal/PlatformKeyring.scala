package morphir.appkit
package internal

import kyo.*

/** java-keyring is JVM-only. */
private[appkit] object PlatformKeyring extends KeyringGet:
  private val detail =
    "javaKeychain is not linked on Scala Native"

  def password(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
    val _ = (service, account)
    Abort.fail(SecretError.NotAvailable(detail))

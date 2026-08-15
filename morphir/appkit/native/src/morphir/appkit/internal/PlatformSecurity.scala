package morphir.appkit
package internal

import kyo.*

/** Scala Native has no process floor for `security` yet. */
private[appkit] object PlatformSecurity extends SecurityCli:
  private val detail =
    "macOsKeychain process spawn is not linked on Scala Native"

  def findGenericPassword(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
    val _ = (service, account)
    Abort.fail(SecretError.NotAvailable(detail))

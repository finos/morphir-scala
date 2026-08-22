package morphir.appkit
package internal

import kyo.*

/** java-keyring is JVM-only. */
private[appkit] object PlatformKeyring extends KeyringAccess:
  private val detail =
    "javaKeychain is not linked on Scala.js"

  def get(service: String, account: String): String < Async = unavailable(service, account)

  def set(service: String, account: String, value: String): Unit < Async =
    val _ = value
    unavailable(service, account)

  def delete(service: String, account: String): Unit < Async = unavailable(service, account)

  private def unavailable[A](service: String, account: String): A < Async =
    val _ = (service, account)
    Sync.defer(throw SecretException.NotAvailable(detail))

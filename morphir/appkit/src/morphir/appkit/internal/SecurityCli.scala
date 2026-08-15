package morphir.appkit
package internal

import kyo.*

/** Runs `security find-generic-password`. Tests inject a fake. */
private[appkit] trait SecurityCli:
  def findGenericPassword(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async)

private[appkit] object SecurityCli:
  def succeed(secret: String): SecurityCli =
    new SecurityCli:
      def findGenericPassword(service: String, account: String) =
        val _ = (service, account)
        Present(secret)

  def missing: SecurityCli =
    new SecurityCli:
      def findGenericPassword(service: String, account: String) =
        val _ = (service, account)
        Absent

  def fail(error: SecretError): SecurityCli =
    new SecurityCli:
      def findGenericPassword(service: String, account: String) =
        val _ = (service, account)
        Abort.fail(error)

  def platform: SecurityCli = PlatformSecurity

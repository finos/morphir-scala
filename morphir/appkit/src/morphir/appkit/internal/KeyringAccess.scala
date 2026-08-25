package morphir.appkit
package internal

import kyo.*
import scala.collection.mutable

/** Accesses passwords in the system keyring. Tests use its in-memory fake. */
private[appkit] trait KeyringAccess:
  def get(service: String, account: String): String < Async
  def set(service: String, account: String, value: String): Unit < Async
  def delete(service: String, account: String): Unit < Async

private[appkit] object KeyringAccess:

  enum Operation:
    case Get, Set, Delete

  enum Call:
    case Get(service: String, account: String)
    case Set(service: String, account: String)
    case Delete(service: String, account: String)

  final class Fake private[appkit] (entries: mutable.Map[(String, String), String]) extends KeyringAccess:
    private val recordedCalls = mutable.ArrayBuffer.empty[Call]
    private val failures      = mutable.Map.empty[Operation, Throwable]

    def calls: Seq[Call] = recordedCalls.toSeq

    def fail(operation: Operation, failure: Throwable): Unit =
      failures.update(operation, failure)

    def get(service: String, account: String): String < Async =
      Sync.defer {
        recordedCalls.addOne(Call.Get(service, account))
        failures.get(Operation.Get).foreach(throw _)
        entries.getOrElse((service, account), "")
      }

    def set(service: String, account: String, value: String): Unit < Async =
      Sync.defer {
        recordedCalls.addOne(Call.Set(service, account))
        failures.get(Operation.Set).foreach(throw _)
        entries.update((service, account), value)
      }

    def delete(service: String, account: String): Unit < Async =
      Sync.defer {
        recordedCalls.addOne(Call.Delete(service, account))
        failures.get(Operation.Delete).foreach(throw _)
        entries.remove((service, account))
        ()
      }

  def fake(entries: (String, String, String)*): Fake =
    new Fake(mutable.Map.from(entries.map { case (service, account, value) => (service, account) -> value }))

  def platform: KeyringAccess = PlatformKeyring

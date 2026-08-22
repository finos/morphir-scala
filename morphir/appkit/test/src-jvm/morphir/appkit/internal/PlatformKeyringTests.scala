package morphir.appkit
package internal

import com.github.javakeyring.KeyringStorageType
import kyo.test.*

class PlatformKeyringTests extends Test[Any]:

  private final class FakeProbe(
      names: Set[String] = Set.empty,
      failure: Throwable | Null = null
  ) extends PlatformKeyring.LinuxServiceProbe:
    var closed = false

    def serviceNames: Set[String] =
      if failure == null then names else throw failure

    def close(): Unit = closed = true

  "Linux keyring service probe" - {
    "selects the verified Secret Service backend and closes the probe" in {
      val probe = FakeProbe(Set("org.freedesktop.secrets", "org.kde.kwalletd5"))

      assert(PlatformKeyring.linuxStorage(probe) == KeyringStorageType.GNOME_KEYRING)
      assert(probe.closed)
    }

    "selects the verified KWallet backend and closes the probe" in {
      val probe = FakeProbe(Set("org.kde.kwalletd5"))

      assert(PlatformKeyring.linuxStorage(probe) == KeyringStorageType.KWALLET)
      assert(probe.closed)
    }

    "fails unavailable before backend construction when neither service exists" in {
      val probe                      = FakeProbe()
      var observed: Throwable | Null = null
      try PlatformKeyring.linuxStorage(probe)
      catch case error: Throwable => observed = error

      assert(observed.isInstanceOf[SecretException.NotAvailable])
      assert(probe.closed)
    }

    "closes the probe when service discovery fails" in {
      val failure                    = RuntimeException("probe-failure-sentinel")
      val probe                      = FakeProbe(failure = failure)
      var observed: Throwable | Null = null
      try PlatformKeyring.linuxStorage(probe)
      catch case error: Throwable => observed = error

      assert(observed eq failure)
      assert(probe.closed)
    }
  }
end PlatformKeyringTests

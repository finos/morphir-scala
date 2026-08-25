package morphir.appkit
package internal

import com.github.javakeyring.Keyring
import com.github.javakeyring.KeyringStorageType
import com.github.javakeyring.PasswordAccessException
import java.util.Locale
import kyo.*
import org.freedesktop.dbus.connections.impl.DBusConnectionBuilder
import org.freedesktop.dbus.interfaces.DBus

/** JVM java-keyring backend. A missing entry is represented by an empty value. */
private[appkit] object PlatformKeyring extends KeyringAccess:
  private val SecretService = "org.freedesktop.secrets"
  private val KWallet       = "org.kde.kwalletd5"

  private[appkit] trait LinuxServiceProbe extends AutoCloseable:
    def serviceNames: Set[String]

  private object LinuxServiceProbe:
    def open(): LinuxServiceProbe =
      val connection = DBusConnectionBuilder.forSessionBus().withShared(false).build()
      new LinuxServiceProbe:
        def serviceNames: Set[String] =
          val bus = connection.getRemoteObject(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            classOf[DBus]
          )
          (bus.ListNames() ++ bus.ListActivatableNames()).toSet

        def close(): Unit = connection.close()

  def get(service: String, account: String): String < Async =
    Sync.defer(read(service, account))

  def set(service: String, account: String, value: String): Unit < Async =
    Sync.defer(withKeyring(_.setPassword(service, account, value)))

  def delete(service: String, account: String): Unit < Async =
    Sync.defer(withKeyring(_.deletePassword(service, account)))

  private def read(service: String, account: String): String =
    try
      withKeyring { keyring =>
        val secret = keyring.getPassword(service, account)
        if secret == null then "" else secret
      }
    catch
      case _: PasswordAccessException => ""

  private def withKeyring[A](run: Keyring => A): A =
    val keyring =
      if isLinux then Keyring.create(linuxStorage(LinuxServiceProbe.open()))
      else Keyring.create()
    try run(keyring)
    finally keyring.close()

  private def isLinux: Boolean =
    java.lang.System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("linux")

  private[appkit] def linuxStorage(probe: LinuxServiceProbe): KeyringStorageType =
    try
      val names = probe.serviceNames
      if names.contains(SecretService) then KeyringStorageType.GNOME_KEYRING
      else if names.contains(KWallet) then KeyringStorageType.KWALLET
      else throw SecretException.NotAvailable("System keyring service is unavailable")
    finally probe.close()

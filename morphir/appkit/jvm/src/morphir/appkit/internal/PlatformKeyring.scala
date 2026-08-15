package morphir.appkit
package internal

import com.github.javakeyring.Keyring
import com.github.javakeyring.PasswordAccessException
import kyo.*

/** JVM java-keyring backend. Missing entries are Absent. */
private[appkit] object PlatformKeyring extends KeyringGet:
  def password(service: String, account: String): Maybe[String] < (Abort[SecretException] & Async) =
    Sync.defer(run(service, account)).map {
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(SecretException.LookupFailed(err.getMessage))
    }

  private def run(service: String, account: String): Result[SecretException, Maybe[String]] =
    try
      val keyring = Keyring.create()
      try
        val secret = keyring.getPassword(service, account)
        if secret == null || secret.isEmpty then Result.Success(Absent)
        else Result.Success(Present(secret))
      finally
        keyring.close()
    catch
      case _: PasswordAccessException =>
        Result.Success(Absent)
      case e: Exception =>
        Result.Failure(SecretException.LookupFailed(e.getMessage))

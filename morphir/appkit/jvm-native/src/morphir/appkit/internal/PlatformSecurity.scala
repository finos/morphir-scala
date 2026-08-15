package morphir.appkit
package internal

import java.nio.charset.StandardCharsets
import kyo.*
import scala.jdk.CollectionConverters.*

/** Spawns `security find-generic-password` on the JVM and Scala Native. Exit 44 is Absent. */
private[appkit] object PlatformSecurity extends SecurityCli:
  private val NotFound = 44

  def findGenericPassword(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
    Sync.defer(run(service, account)).map {
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(SecretError.LookupFailed(err.getMessage))
    }

  private def run(service: String, account: String): Result[SecretError, Maybe[String]] =
    try
      val command = Seq("security", "find-generic-password", "-s", service, "-a", account, "-w").asJava
      val process = new ProcessBuilder(command).start()
      val out     = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      val err     = String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
      val code    = process.waitFor()
      if code == 0 then
        val trimmed = out.trim
        if trimmed.isEmpty then Result.Success(Absent) else Result.Success(Present(trimmed))
      else if code == NotFound then Result.Success(Absent)
      else Result.Failure(SecretError.LookupFailed(detail(code, err, out)))
    catch
      case e: java.io.IOException =>
        Result.Failure(SecretError.NotAvailable(s"security is not installed or could not be started: ${e.getMessage}"))
      case e: Exception =>
        Result.Failure(SecretError.LookupFailed(e.getMessage))

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"security exited $code: $fromErr"
    else if fromOut.nonEmpty then s"security exited $code: $fromOut"
    else s"security exited $code"

package morphir.appkit
package internal

import kyo.*
import scala.scalajs.js

/** Spawns `security find-generic-password` on Node.js. Exit 44 is Absent. */
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
      val child  = js.Dynamic.global.require("child_process")
      val args   = js.Array("find-generic-password", "-s", service, "-a", account, "-w")
      val result = child.spawnSync("security", args, js.Dynamic.literal(encoding = "utf8"))
      val error  = result.error
      if !js.isUndefined(error) && error != null then
        Result.Failure(SecretError.NotAvailable(s"security is not installed or could not be started: ${error.message}"))
      else
        val code = result.status.asInstanceOf[js.UndefOr[Int]].toOption.getOrElse(-1)
        val out  = stringOf(result.stdout)
        val err  = stringOf(result.stderr)
        if code == 0 then
          val trimmed = out.trim
          if trimmed.isEmpty then Result.Success(Absent) else Result.Success(Present(trimmed))
        else if code == NotFound then Result.Success(Absent)
        else Result.Failure(SecretError.LookupFailed(detail(code, err, out)))
    catch
      case e: Exception =>
        Result.Failure(SecretError.LookupFailed(e.getMessage))

  private def stringOf(value: js.Dynamic): String =
    if js.isUndefined(value) || value == null then ""
    else value.toString

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"security exited $code: $fromErr"
    else if fromOut.nonEmpty then s"security exited $code: $fromOut"
    else s"security exited $code"

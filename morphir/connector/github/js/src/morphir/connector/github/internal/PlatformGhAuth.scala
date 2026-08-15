package morphir.connector.github
package internal

import kyo.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Spawns `gh auth token` on Node.js. Missing binary or a non-zero exit is Unauthorized. */
private[github] object PlatformGhAuth extends GhAuth:
  def stdout(args: Chunk[String]): String < (Abort[GithubError] & Async) =
    Sync.defer(run(args)).map {
      case Result.Success(out) => out
      case Result.Failure(err) => Abort.fail(err)
      case Result.Panic(err)   => Abort.fail(GithubError.Unauthorized(err.getMessage))
    }

  private def run(args: Chunk[String]): Result[GithubError, String] =
    try
      val child  = js.Dynamic.global.require("child_process")
      val result = child.spawnSync(
        "gh",
        args.toSeq.toJSArray,
        js.Dynamic.literal(encoding = "utf8")
      )
      val error = result.error
      if !js.isUndefined(error) && error != null then
        Result.Failure(GithubError.Unauthorized(s"gh is not installed or could not be started: ${error.message}"))
      else
        val code = result.status.asInstanceOf[js.UndefOr[Int]].toOption.getOrElse(-1)
        val out  = stringOf(result.stdout)
        val err  = stringOf(result.stderr)
        if code == 0 then Result.Success(out)
        else Result.Failure(GithubError.Unauthorized(detail(code, err, out)))
    catch
      case e: Exception =>
        Result.Failure(GithubError.Unauthorized(e.getMessage))

  private def stringOf(value: js.Dynamic): String =
    if js.isUndefined(value) || value == null then ""
    else value.toString

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"gh auth token exited $code: $fromErr"
    else if fromOut.nonEmpty then s"gh auth token exited $code: $fromOut"
    else s"gh auth token exited $code"

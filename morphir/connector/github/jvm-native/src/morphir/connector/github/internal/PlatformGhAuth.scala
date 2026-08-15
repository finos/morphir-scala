package morphir.connector.github
package internal

import java.nio.charset.StandardCharsets
import kyo.*
import scala.jdk.CollectionConverters.*

/** Spawns `gh auth token` on the JVM and Scala Native. Missing binary or a non-zero exit is Unauthorized. */
private[github] object PlatformGhAuth extends GhAuth:
  def stdout(args: Chunk[String]): String < (Abort[GithubError] & Async) =
    Sync.defer(run(args)).map {
      case Result.Success(out) => out
      case Result.Failure(err) => Abort.fail(err)
      case Result.Panic(err)   => Abort.fail(GithubError.Unauthorized(err.getMessage))
    }

  private def run(args: Chunk[String]): Result[GithubError, String] =
    try
      val command = ("gh" +: args.toSeq).asJava
      val process = new ProcessBuilder(command).start()
      val out     = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      val err     = String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
      val code    = process.waitFor()
      if code == 0 then Result.Success(out)
      else Result.Failure(GithubError.Unauthorized(detail(code, err, out)))
    catch
      case e: java.io.IOException =>
        Result.Failure(GithubError.Unauthorized(s"gh is not installed or could not be started: ${e.getMessage}"))
      case e: Exception =>
        Result.Failure(GithubError.Unauthorized(e.getMessage))

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"gh auth token exited $code: $fromErr"
    else if fromOut.nonEmpty then s"gh auth token exited $code: $fromOut"
    else s"gh auth token exited $code"

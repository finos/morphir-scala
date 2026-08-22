package morphir.connector.github
package internal

import java.nio.charset.StandardCharsets
import kyo.*

/** Spawns `gh auth token`. Missing binary, launch permission, or non-zero exit is Unauthorized. */
private[github] object PlatformGhAuth extends GhAuth:
  def stdout(args: Chunk[String]): String < (Abort[GitHubException] & Async) =
    run("gh", args)

  private[github] def forProgram(program: String): GhAuth =
    new GhAuth:
      def stdout(args: Chunk[String]) = run(program, args)

  private def run(program: String, args: Chunk[String]): String < (Abort[GitHubException] & Async) =
    Abort.recover[CommandException](exception => Abort.fail(commandFailure(exception))) {
      Scope.run {
        for
          process     <- Command(program +: args.toSeq*).spawn
          stdoutFiber <- Fiber.init(Scope.run(process.stdout.run))
          stderrFiber <- Fiber.init(Scope.run(process.stderr.run))
          exitCode    <- process.waitFor
          stdout      <- stdoutFiber.get
          stderr      <- stderrFiber.get
          out = String(stdout.toArray, StandardCharsets.UTF_8)
          err = String(stderr.toArray, StandardCharsets.UTF_8)
          _ <-
            if exitCode.toInt == 0 then Sync.defer(())
            else Abort.fail(GitHubException.Unauthorized(detail(exitCode.toInt, err, out)))
        yield out
      }
    }

  private def commandFailure(exception: CommandException): GitHubException.Unauthorized =
    exception match
      case _: ProgramNotFoundException | _: PermissionDeniedException =>
        GitHubException.Unauthorized("gh is not installed or could not be started")
      case _ =>
        GitHubException.Unauthorized("gh auth token could not be started")

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"gh auth token exited $code: $fromErr"
    else if fromOut.nonEmpty then s"gh auth token exited $code: $fromOut"
    else s"gh auth token exited $code"

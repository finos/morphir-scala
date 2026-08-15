package morphir.appkit
package internal

import java.nio.charset.StandardCharsets
import kyo.*

/** Spawns `security find-generic-password`. Exit 44 is Absent. */
private[appkit] object PlatformSecurity extends SecurityCli:
  private val NotFound = 44

  def findGenericPassword(service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
    run("security", service, account)

  private[appkit] def forProgram(program: String): SecurityCli =
    new SecurityCli:
      def findGenericPassword(service: String, account: String) = run(program, service, account)

  private def run(program: String, service: String, account: String): Maybe[String] < (Abort[SecretError] & Async) =
    Abort.recover[CommandException](exception => Abort.fail(commandFailure(exception))) {
      Scope.run {
        for
          process     <- Command(program, "find-generic-password", "-s", service, "-a", account, "-w").spawn
          stdoutFiber <- Fiber.init(Scope.run(process.stdout.run))
          stderrFiber <- Fiber.init(Scope.run(process.stderr.run))
          exitCode    <- process.waitFor
          stdout      <- stdoutFiber.get
          stderr      <- stderrFiber.get
          out = String(stdout.toArray, StandardCharsets.UTF_8)
          err = String(stderr.toArray, StandardCharsets.UTF_8)
          value <-
            if exitCode.toInt == 0 then
              Sync.defer {
                val trimmed = out.trim
                if trimmed.isEmpty then Absent else Present(trimmed)
              }
            else if exitCode.toInt == NotFound then Sync.defer(Absent)
            else Abort.fail(SecretError.LookupFailed(detail(exitCode.toInt, err, out)))
        yield value
      }
    }

  private def commandFailure(exception: CommandException): SecretError =
    exception match
      case _: ProgramNotFoundException | _: PermissionDeniedException =>
        SecretError.NotAvailable(s"security is not installed or could not be started: ${exception.getMessage}")
      case other =>
        SecretError.LookupFailed(other.getMessage)

  private def detail(code: Int, err: String, out: String): String =
    val fromErr = err.trim
    val fromOut = out.trim
    if fromErr.nonEmpty then s"security exited $code: $fromErr"
    else if fromOut.nonEmpty then s"security exited $code: $fromOut"
    else s"security exited $code"

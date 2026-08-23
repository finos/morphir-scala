package morphir.appkit
package internal

import java.nio.charset.StandardCharsets
import kyo.*

/** Spawns `security find-generic-password`. Exit 44 is Absent. */
private[appkit] object PlatformSecurity extends SecurityCli:
  private val NotFound = 44

  def findGenericPassword(service: String, account: String): Maybe[String] < (Abort[SecretException] & Async) =
    run("security", service, account)

  private[appkit] def forProgram(program: String): SecurityCli =
    new SecurityCli:
      def findGenericPassword(service: String, account: String) = run(program, service, account)

  private def run(program: String, service: String, account: String): Maybe[String] < (Abort[SecretException] & Async) =
    Abort.recover[CommandException](exception => Abort.fail(commandFailure(exception))) {
      Scope.run {
        for
          process     <- Command(program, "find-generic-password", "-s", service, "-a", account, "-w").spawn
          stdoutFiber <- Fiber.init(Scope.run(process.stdout.run))
          stderrFiber <- Fiber.init(Scope.run(process.stderr.run))
          exitCode    <- process.waitFor
          stdout      <- stdoutFiber.get
          _           <- stderrFiber.get
          value       <-
            if exitCode.toInt == 0 then
              Sync.defer {
                val out   = String(stdout.toArray, StandardCharsets.UTF_8)
                val value = stripTrailingLineEnding(out)
                if value.isEmpty then Absent else Present(value)
              }
            else if exitCode.toInt == NotFound then Sync.defer(Absent)
            else Abort.fail(SecretException.LookupFailed(s"security exited ${exitCode.toInt}"))
        yield value
      }
    }

  private def commandFailure(exception: CommandException): SecretException =
    exception match
      case _: ProgramNotFoundException | _: PermissionDeniedException =>
        SecretException.NotAvailable("security is not installed or could not be started")
      case _ =>
        SecretException.LookupFailed("security command could not be started")

  private[appkit] def stripTrailingLineEnding(value: String): String =
    if value.endsWith("\r\n") then value.dropRight(2)
    else if value.endsWith("\n") then value.dropRight(1)
    else value

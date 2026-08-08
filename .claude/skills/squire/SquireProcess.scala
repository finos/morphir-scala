//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala]

import java.nio.charset.StandardCharsets
import kyo.*

final case class ProcessRequest(argv: Chunk[String], cwd: Maybe[Path] = Absent) derives CanEqual

final case class ProcessResult(request: ProcessRequest, exitCode: Int, stdout: String, stderr: String) derives CanEqual

trait ProcessRunner:
  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError])

object LiveProcessRunner extends ProcessRunner:
  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    Abort.recover[CommandException](exception =>
      Abort.fail(
        SquireError.Failure(
          "process",
          "could not launch external command",
          Present(exception.getMessage)
        )
      )
    ) {
      Scope.run {
        for
          process <- command(request).spawn
          stdoutFiber <- Fiber.init(Scope.run(process.stdout.run))
          stderrFiber <- Fiber.init(Scope.run(process.stderr.run))
          exitCode <- process.waitFor
          stdout <- stdoutFiber.get
          stderr <- stderrFiber.get
        yield ProcessResult(
          request,
          exitCode.toInt,
          new String(stdout.toArray, StandardCharsets.UTF_8),
          new String(stderr.toArray, StandardCharsets.UTF_8)
        )
      }
    }

  private def command(request: ProcessRequest): Command =
    request.cwd match
      case Present(path) => Command(request.argv.toSeq*).cwd(path)
      case Absent        => Command(request.argv.toSeq*)

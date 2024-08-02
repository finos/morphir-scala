package millbuild.jsruntime
import scala.util.Properties.isWin
import mill.api._
import mill.util.Jvm

import os.SubProcess

trait JsRuntime {
  import JsRuntime._
  def toolName: String

  def findTool(name: String): String =
    whereIs(name)

  def executable: String =
    findTool(toolName)

  def runSubprocess(entryPoint: String, args: Seq[String], envArgs: Map[String, String], workingDir: os.Path)(implicit
      ctx: Ctx
  ): Unit = {
    val commandArgs = Vector(executable) ++ args
    println(s"CommandArgs: $commandArgs")
    val process: SubProcess = Jvm.spawnSubprocessWithBackgroundOutputs(
      commandArgs,
      envArgs,
      workingDir,
      backgroundOutputs = None
    )
    println(s"Createds process: ${process}")

    val shutdownHook = new Thread("subprocess-shutdown") {
      override def run(): Unit = {
        System.err.println(s"Host executable for $toolName shutdown. Forcefully destroying subprocess ...")
        process.destroy()
      }
    }
    Runtime.getRuntime().addShutdownHook(shutdownHook)
    try
      process.waitFor()
    catch {
      case e: InterruptedException =>
        System.err.println("Interrupted. Forcefully destroying subprocess ...")
        process.destroy()
        // rethrow
        throw e
    } finally
      Runtime.getRuntime().removeShutdownHook(shutdownHook)
    if (process.exitCode() == 0) ()
    else throw new Exception("Interactive Subprocess Failed (exit code " + process.exitCode() + ")")

  }
}

object JsRuntime {
  case object NodeJs extends JsRuntime {
    def toolName: String = "node"
  }

  def whereIs(name: String) = {
    val commandArgs =
      if (isWin) {
        Vector("where", name)
      } else {
        Vector("whereis", name)
      }

    val result = os.proc(commandArgs).call()

    val location =
      if (isWin) {
        result.out.lines().headOption.flatMap(_.trim().split(" ").headOption)
      } else {

        result.out.lines().headOption.flatMap { line =>
          val parts = line.trim().split(" ")
          parts match {
            case Array(_, loc, _*) => Some(loc)
            case _                 => None
          }

        }

      }

    location match {
      case None      => throw new Exception(s"Failed to locate tool: $name")
      case Some(loc) => loc
    }
  }
}

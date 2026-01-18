package millbuild.jsruntime
import scala.util.Properties.isWin
import mill.api.*

import os.SubProcess

trait JsRuntime {
  import JsRuntime._
  def toolName: String

  def findTool(name: String): String =
    whereIs(name)

  def executable: String =
    findTool(toolName)

  def runSubprocess(entryPoint: String, args: Seq[String], envArgs: Map[String, String], workingDir: os.Path): Unit = {
    val commandArgs = Vector(executable) ++ args
    println(s"CommandArgs: $commandArgs")
    val result = os.proc(commandArgs).call(cwd = workingDir, env = envArgs)
    if (result.exitCode != 0) {
      throw new Exception("Interactive Subprocess Failed (exit code " + result.exitCode + ")")
    }
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

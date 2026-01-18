package millbuild.util
import scala.util.Properties.isWin

object ProcessHelper {
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

object Collections {
  implicit class SeqOps[+A](private val self: Seq[A]) extends AnyVal {
    def appendIf[A1 >: A](cond: Boolean)(items: A1*)      = if (cond) self ++ items else self
    def appendWhen[A1 >: A](cond: => Boolean)(items: A1*) = if (cond) self ++ items else self

    def when[A1 >: A](cond: Boolean)(items: A1*) = if (cond) items else Seq.empty[A1]
  }
}

object Jvm {
  /**
   * Runs a subprocess with the given command arguments.
   */
  def runSubprocess(
      commandArgs: Seq[String],
      envArgs: Map[String, String],
      workingDir: os.Path
  ): Unit = {
    val result = os.proc(commandArgs).call(cwd = workingDir, env = envArgs)
    if (result.exitCode != 0) {
      throw new Exception(s"Subprocess failed with exit code ${result.exitCode}")
    }
  }
}

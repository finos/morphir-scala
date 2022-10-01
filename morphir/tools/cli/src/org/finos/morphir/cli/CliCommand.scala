package org.finos.morphir.cli
import java.nio.file.{Path, Paths}
sealed trait CliCommand extends Product with Serializable
object CliCommand:
  case object About                        extends CliCommand
  final case class Elm(args: List[String]) extends CliCommand
  final case class Init()                  extends CliCommand
  final case class Setup()                 extends CliCommand
  final case class Workspace()             extends CliCommand

  object Elm:
    sealed trait ElmSubCommand                                extends CliCommand
    final case class Develop(port: Int = Develop.defaultPort) extends ElmSubCommand
    final case class Make(projectDir: Path = Paths.get("."), output: Path = Paths.get("."), typesOnly: Boolean = false)
        extends ElmSubCommand
    final case class Gen() extends ElmSubCommand

    object Develop:
      val defaultPort: Int = 3000

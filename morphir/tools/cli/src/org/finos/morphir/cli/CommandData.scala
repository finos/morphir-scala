package org.finos.morphir.cli

sealed trait CommandData extends Product with Serializable
object CommandData:
  final case class Elm(args: List[String]) extends CommandData
  final case class Init()                  extends CommandData
  case object Setup                        extends CommandData
  final case class Workspace()             extends CommandData

  object Elm:
    sealed trait ElmSubCommand                                extends CommandData
    final case class Develop(port: Int = Develop.defaultPort) extends ElmSubCommand
    final case class Make(projectDir: os.Path = os.pwd, output: os.Path = os.pwd, typesOnly: Boolean = false)
        extends ElmSubCommand
    final case class Gen() extends ElmSubCommand

    object Develop:
      val defaultPort: Int = 3000

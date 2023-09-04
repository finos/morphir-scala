package org.finos.morphir.command

import zio._
import java.nio.file.Path

trait MorphirCommandModule extends SetupCommandModule with ElmCommandModule {
  import MorphirCommand.*
  given morphirCommandExecutor(using CommandExecutor[ElmMake]): CommandExecutor[MorphirCommand]
  with {
    extension (command: MorphirCommand)
      def execute(): Task[Unit] = command match {
        case cmd @ MorphirCommand.Setup()          => ??? // cmd.execute()
        case cmd @ MorphirCommand.ElmInit()        => ??? // cmd.execute()
        case cmd @ MorphirCommand.ElmMake(_, _, _) => CommandExecutor[ElmMake].execute(cmd)()
      }
  }
}

trait SetupCommandModule {
  given CommandExecutor[MorphirCommand.Setup] with {
    extension (command: MorphirCommand.Setup)
      def execute(): Task[Unit] =
        for {
          _ <- Console.printLine("Setup command executed")
          _ <- Console.printLine(s"With command: $command")
        } yield ()
  }

}

trait ElmCommandModule {
  given CommandExecutor[MorphirCommand.ElmInit] with {
    extension (command: MorphirCommand.ElmInit)
      def execute(): Task[Unit] =
        for {
          _ <- Console.printLine("Elm init command executed")
          _ <- Console.printLine(s"With command: $command")
        } yield ()
  }

  given CommandExecutor[MorphirCommand.ElmMake] with {
    extension (command: MorphirCommand.ElmMake)
      def execute(): Task[Unit] =
        for {
          _ <- Console.printLine("Elm make command executed")
          _ <- Console.printLine(s"With command: $command")
        } yield ()
  }
}





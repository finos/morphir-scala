package org.finos.morphir.command

import zio._

trait CommandExecutor[C] {
  extension (command: C) def execute(): Task[Unit]
}

object CommandExecutor extends MorphirCommandModule {
  def apply[C](using executor: CommandExecutor[C]): CommandExecutor[C] = executor
}

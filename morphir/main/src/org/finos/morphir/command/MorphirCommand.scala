package org.finos.morphir.command
import zio._
import java.nio.file.Path

enum MorphirCommand {
  case Setup()
  case ElmInit()
  case ElmMake(projectDir: Path, output: Path, fallbackCli: Boolean = false)
}

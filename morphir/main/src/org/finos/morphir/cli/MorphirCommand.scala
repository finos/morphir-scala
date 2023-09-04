package org.finos.morphir.cli
import zio._
import java.nio.file.Path

enum MorphirCommand {
  case Setup(morphirHomeDir: Path)
  case ElmInit(morphirHomeDir: Path, projectDir: Path)
  case ElmMake(projectDir: Path, output: Path, fallbackCli: Boolean = false)
}

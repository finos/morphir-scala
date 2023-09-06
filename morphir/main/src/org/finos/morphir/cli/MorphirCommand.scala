package org.finos.morphir.cli
import zio._
import java.nio.file.Path

enum MorphirCommand {
  case Develop(port: Int, host: String, projectDir: Path)
  case Setup(morphirHomeDir: Path)
  case Test(irFiles: List[Path])
  case ElmInit(morphirHomeDir: Path, projectDir: Path)
  case ElmMake(projectDir: Path, output: Path, fallbackCli: Boolean = false)
  case ElmRestore(elmHome: Path, projectDir: Path)
}

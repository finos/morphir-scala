package org.finos.morphir.service

import org.finos.morphir.util.vfile.*
import zio.*

trait MorphirBundlePlatformSpecific {
  val live: ULayer[MorphirBundle] = ZLayer.succeed(MorphirBundleLive)

  object MorphirBundleLive extends MorphirBundle {
    def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit] =
      for {
        _ <- Console.printLine("Bundle command executing")
        _ <- Console.printLine(s"\toutputBundleIRFilePath: $outputBundleIRFilePath")
        _ <- Console.printLine(s"\tirFiles: $irFiles")
        _ <- Console.printLine("Bundle command executed")
      } yield ()
  }
}

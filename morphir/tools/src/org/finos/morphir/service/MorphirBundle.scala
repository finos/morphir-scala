package org.finos.morphir.service

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.util.vfile.*
import scala.io.Source
import zio.*
import zio.json.*

trait MorphirBundle {
  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit]
}

object MorphirBundle {

    // TODO: Possibly refactor when FileIO operations are completed
    def loadDistributionFromFileZIO(fileName: String): Task[Distribution] =
      for {
        fileContents <- ZIO.readFile(fileName)
        morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
          .mapError(error => throw new Exception(s"Parsing Error: $error"))
      } yield morphirIRFile.distribution

    // TODO: Possibly refactor when FileIO operations are completed
    def writeDistrubtionToFileZIO(bundle: Distribution, path: VPath): Task[VPath] =
      for {
        morphirIRFile <- ZIO.attempt { MorphirIRFile(MorphirIRVersion.Default, bundle) }
        irJson        <- ZIO.attempt { morphirIRFile.toJson }
        path          <- ZIO.attempt { Files.write(path.path.toNioPath, irJson.getBytes(StandardCharsets.UTF_8)) }
      } yield VPath(path)
  }

  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.bundle(outputBundleIRFilePath, irFiles))
}

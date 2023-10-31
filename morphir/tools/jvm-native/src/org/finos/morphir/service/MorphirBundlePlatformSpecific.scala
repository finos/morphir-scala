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

trait MorphirBundlePlatformSpecific {
  val live: ULayer[MorphirBundle] = ZLayer.succeed(MorphirBundleLive)

  object MorphirBundleLive extends MorphirBundle {
    def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit] =
      for {
        _             <- Console.printLine("Bundle command executing")
        _             <- Console.printLine(s"\toutputBundleIRFilePath: $outputBundleIRFilePath")
        _             <- Console.printLine(s"\tirFiles: $irFiles")
        distributions <- ZIO.collectAll { irFiles.map { irFile => loadDistributionFromFileZIO(irFile.toString) } }
        bundle        <- ZIO.attempt { Distribution.toBundle(distributions: _*) }
        writtenPath   <- writeDistrubtionToFileZIO(bundle, outputBundleIRFilePath)
        _             <- Console.printLine(s"\tBundle IR file created: $writtenPath")
        _             <- Console.printLine("Bundle command executed")
      } yield ()

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
}

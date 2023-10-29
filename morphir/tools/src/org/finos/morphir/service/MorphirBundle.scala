package org.finos.morphir.service

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.util.vfile.*
import org.finos.morphir.runtime.MorphirRuntimeError.*
import scala.io.Source
import zio.*
import zio.json.*

trait MorphirBundle {
  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit]
}

object MorphirBundle {
  val live: ULayer[MorphirBundle] = ZLayer.succeed(MorphirBundleLive)

  object MorphirBundleLive extends MorphirBundle {
    def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit] =
      for {
        _             <- Console.printLine("Bundle command executing")
        _             <- Console.printLine(s"\toutputBundleIRFilePath: $outputBundleIRFilePath")
        _             <- Console.printLine(s"\tirFiles: $irFiles")
        distributions <- ZIO.collectAll { irFiles.map { irFile => loadDistributionFromFileZIO(irFile.toString) } }
        bundle        <- ZIO.attempt { Distribution.toBundle(distributions: _*) }
        morphirIRFile <- ZIO.attempt { MorphirIRFile(MorphirIRVersion.Default, bundle) }
        writtenPath   <- writeDistrubtionToFileZIO(morphirIRFile, outputBundleIRFilePath)
        _             <- Console.printLine(s"\tBundle IR file created: $writtenPath")
        _             <- Console.printLine("Bundle command executed")
      } yield ()

    // TODO: Possibly refactor when FileIO operations are completed
    def loadDistributionFromFileZIO(fileName: String): Task[Distribution] =
      for {
        fileContents <- ZIO.readFile(fileName)
        morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
          .mapError(s => throw new Exception(s"Parsing Error: $s"))
      } yield morphirIRFile.distribution

    // TODO: Possibly refactor when FileIO operations are completed
    def writeDistrubtionToFileZIO(file: MorphirIRFile, path: VPath): Task[VPath] = {
      import org.finos.morphir.ir.json.MorphirJsonSupport.*
      for {
        newFileJson <- ZIO.attempt { file.toJson }
        path        <- ZIO.attempt { Files.write(path.path.toNioPath, newFileJson.getBytes(StandardCharsets.UTF_8)) }
      } yield VPath(path)
    }
  }

  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.bundle(outputBundleIRFilePath, irFiles))
}

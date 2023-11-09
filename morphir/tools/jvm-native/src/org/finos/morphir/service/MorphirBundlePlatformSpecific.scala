package org.finos.morphir.service

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.util.vfile.*
import zio.*
import zio.json.*

trait MorphirBundlePlatformSpecific {
  val live: ULayer[MorphirBundle] = ZLayer.succeed(MorphirBundleLive)

  object MorphirBundleLive extends MorphirBundle {

    def bundle(outputPath: VPath, irFiles: List[VPath]): Task[Unit] =
      for {
        _             <- Console.printLine("Bundle command executing")
        _             <- Console.printLine(s"\toutputPath: $outputPath")
        _             <- Console.printLine(s"\tirFiles: $irFiles")
        distributions <- ZIO.collectAll { irFiles.map { irFile => loadDistributionFromFileZIO(irFile.toString) } }
        bundle        <- ZIO.attempt { Distribution.toBundle(distributions: _*) }
        writtenPath   <- writeDistributionToFileZIO(bundle, outputPath)
        _             <- Console.printLine(s"\tBundle Morphir IR file created: $writtenPath")
        _             <- Console.printLine("Bundle command executed")
      } yield ()

    def library(outputDir: VPath, irFiles: List[VPath]): Task[Unit] =
      for {
        _             <- Console.printLine("Library command executing")
        _             <- Console.printLine(s"\toutputDir: $outputDir")
        _             <- Console.printLine(s"\tirFiles: $irFiles")
        distributions <- ZIO.collectAll { irFiles.map { irFile => loadDistributionFromFileZIO(irFile.toString) } }
        libraries     <- ZIO.attempt { Distribution.toLibraries(distributions: _*) }
        libsWithPaths <- ZIO.attempt(createPathsForLibs(libraries, outputDir))
        paths         <- ZIO.foreach(libsWithPaths) { case (lib, path) => writeDistributionToFileZIO(lib, path) }
        _             <- ZIO.foreach(paths) { path => Console.printLine(s"\tLibrary Morphir IR file created: $path") }
        _             <- Console.printLine("Library command executed")
      } yield ()

    def createPathsForLibs(libraries: List[Distribution], outputDir: VPath): List[(Distribution, VPath)] =
      libraries.zip(LazyList.from(1)).map { case (library: Distribution, index: Int) =>
        (library, outputDir / s"morphir-ir${index}.json")
      }

    // TODO: Possibly refactor when FileIO operations are completed
    def loadDistributionFromFileZIO(fileName: String): Task[Distribution] =
      for {
        fileContents <- ZIO.readFile(fileName)
        morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
          .mapError(error => throw new Exception(s"Parsing Error: $error"))
      } yield morphirIRFile.distribution

    // TODO: Possibly refactor when FileIO operations are completed
    def writeDistributionToFileZIO(distribution: Distribution, path: VPath): Task[VPath] =
      for {
        morphirIRFile <- ZIO.attempt { MorphirIRFile(MorphirIRVersion.Default, distribution) }
        irJson        <- ZIO.attempt { morphirIRFile.toJson }
        irFilePath: Path = path.path.toNioPath.toAbsolutePath
        directory: Path  = irFilePath.getParent()
        _       <- ZIO.when(Files.notExists(directory))(ZIO.attempt { Files.createDirectories(directory) })
        outPath <- ZIO.attempt { Files.write(irFilePath, irJson.getBytes(StandardCharsets.UTF_8)) }
      } yield VPath(outPath)
  }
}

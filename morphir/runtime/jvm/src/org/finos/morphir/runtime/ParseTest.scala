package org.finos.morphir.runtime

import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution
import zio.{Runtime, Unsafe, ZIO}
import zio.json.*
import zio.System.os
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object ParseTest {
  def run[E, A](zio: ZIO[Any, E, A]) = Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(zio).getOrThrowFiberFailure()
  }

  def loadDistributionFromFileZIO(fileName: String) = {
    import org.finos.morphir.ir.json.MorphirJsonSupport.*
    val start = System.currentTimeMillis()
    val output =
      run {
        for {
          fileContents <- ZIO.readFile(fileName)
          morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
            .mapError(MorphirIRDecodingError(_))
        } yield morphirIRFile
      }
    val end = System.currentTimeMillis()
    println(s"Total loading time: ${(end - start).toDouble / 1000.toDouble} for ${fileName}")
    output
  }

  def writeDistrubtionToFile(file: MorphirIRFile, path: String) = {
    import org.finos.morphir.ir.json.MorphirJsonSupport.*
    val newFileJson = file.toJson
    Files.write(
      Paths.get(path),
      newFileJson.getBytes(StandardCharsets.UTF_8)
    )
  }

  def main(args: Array[String]): Unit = {
    println("== Starting File Load experiment ==")
    val originalFile = loadDistributionFromFileZIO("examples/morphir-elm-projects/evaluator-tests/morphir-ir.json")
    writeDistrubtionToFile(originalFile, "examples/morphir-elm-projects/evaluator-tests/morphir-ir2.json")
    val newFile = loadDistributionFromFileZIO("examples/morphir-elm-projects/evaluator-tests/morphir-ir2.json")
    println("== Loaded New File ==")
  }
}

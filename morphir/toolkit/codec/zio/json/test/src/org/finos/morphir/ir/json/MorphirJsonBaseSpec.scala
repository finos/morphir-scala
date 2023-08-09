package org.finos.morphir.ir.json

import scala.annotation.nowarn
import java.nio.file.{Paths, Path, Files}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.{test => _, _}
import zio.test._
import zio.test.diff._
import zio.test.diff.Diff._
import zio.json.golden.{goldenTest => _, _}
import zio.json.golden.filehelpers._
import zio.json._
import zio.json.ast._
import java.io.File
import java.io.IOException
import zio.process.Command
import com.deblock.jsondiff.matcher.CompositeJsonMatcher
import com.deblock.jsondiff.matcher.LenientJsonArrayPartialMatcher
import com.deblock.jsondiff.matcher.LenientJsonObjectPartialMatcher
import com.deblock.jsondiff.matcher.LenientNumberPrimitivePartialMatcher
import com.deblock.jsondiff.matcher.StrictPrimitivePartialMatcher
import com.deblock.jsondiff.DiffGenerator
import com.deblock.jsondiff.viewer.OnlyErrorDiffViewer
import com.deblock.jsondiff.viewer.PatchDiffViewer

abstract class MorphirJsonBaseSpec extends MorphirBaseSpec {
  implicit private lazy val diffJsonValue: Diff[Json] = {
    case (x: Json.Obj, y: Json.Obj) =>
      mapDiff[String, Json].diff(x.fields.toMap, y.fields.toMap)

    case (x: Json.Arr, y: Json.Arr) =>
      seqDiff[Json].diff(x.elements, y.elements)

    case (x, y) =>
      if (x == y) DiffResult.Identical(x)
      else DiffResult.Different(x, y)
  }

  @nowarn implicit private lazy val diff: Diff[GoldenSample] = (x: GoldenSample, y: GoldenSample) =>
    Diff[Json].diff(x.samples, y.samples)

  def goldenTest[A: Tag: JsonEncoder](
      gen: Gen[Sized, A]
  )(implicit
      trace: Trace,
      config: GoldenConfiguration
  ): Spec[TestEnvironment, Throwable] = {
    // val _    = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val name = getName[A]
    test(s"golden test for $name") {
      import config.{relativePath, sampleSize}
      for {

        resourceDir <- createGoldenDirectory(s"test/resources/golden/$relativePath")
        fileName = Paths.get(s"$name.json")
        filePath = resourceDir.resolve(fileName)
        assertion <- ZIO.ifZIO(ZIO.attemptBlocking(Files.exists(filePath)))(
          validateTest(resourceDir, name, gen, sampleSize),
          createNewTest(resourceDir, name, gen, sampleSize)
        )
      } yield assertion
    }
  }

  def getRootDir(file: File)(implicit trace: Trace): Task[File] =
    if (file.getName == "test") ZIO.succeed(file)
    else ZIO.attempt(file.getParentFile).flatMap(getRootDir)

  def createGoldenDirectory(pathToDir: String)(implicit trace: Trace): Task[Path] = {
    // val _        = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val rootFile = new File(implicitly[sourcecode.File].value)

    for {
      baseFile <- getRootDir(rootFile)
      goldenDir = new File(baseFile.getParentFile, pathToDir)
      path      = goldenDir.toPath
      _ <- ZIO.attemptBlocking(goldenDir.mkdirs)
    } yield path
  }

  final val jsonMatcher = new CompositeJsonMatcher(
    new LenientJsonArrayPartialMatcher(),  // comparing array using lenient mode (ignore array order and extra items)
    new LenientJsonObjectPartialMatcher(), // comparing object using lenient mode (ignoring extra properties)
    new LenientNumberPrimitivePartialMatcher(
      new StrictPrimitivePartialMatcher()
    ) // comparing primitive types and manage numbers (100.00 == 100)
  )

  implicit class ThrowableExt(t: Throwable) {
    def stackTraceToString: String = {
      val stream = new java.io.ByteArrayOutputStream()
      val writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(stream))
      t.printStackTrace(new java.io.PrintWriter(writer))
      writer.flush
      stream.toString
    }
  }

  def doJsonDiff(expectedJson: String, actualJson: String) = {
    val jsondiff     = DiffGenerator.diff(expectedJson, actualJson, jsonMatcher)
    val errorsResult = OnlyErrorDiffViewer.from(jsondiff)
    // create a patch file text
    // var patch = PatchDiffViewer.from(jsondiff);
    // use the viewer to collect diff data
    val patchFile = PatchDiffViewer.from(jsondiff);
    val patchFileText =
      try
        patchFile.toString
      catch {
        case e: Exception => ???
      }
    (errorsResult.toString, patchFileText)
  }

  def writeContentToFile(path: Path, content: String)(implicit trace: Trace): IO[IOException, Unit] = {
    val jsonString = content

    ZIO.attemptBlockingIO {
      Files.write(path, jsonString.getBytes("UTF-8"))
    }.unit
  }

  private def validateTest[A: JsonEncoder](
      resourceDir: Path,
      name: String,
      gen: Gen[Sized, A],
      sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Throwable, TestResult] = {
    val fileName = Paths.get(s"$name.json")
    val filePath = resourceDir.resolve(fileName)

    for {
      currentSample <- readSampleFromFile(filePath)
      sample        <- generateSample(gen, sampleSize)
      assertion <-
        if (sample == currentSample) {
          ZIO.succeed(assertTrue(sample == currentSample))
        } else {
          val diffFileName      = Paths.get(s"${name}_changed.json")
          val patchFileName     = Paths.get(s"${name}.patch")
          val patchFilePath     = resourceDir.resolve(patchFileName)
          val diffFilePath      = resourceDir.resolve(diffFileName)
          val structureDiff     = "N/A"
          val sampleJson        = sample.toJsonPretty
          val currentSampleJson = currentSample.toJsonPretty

          val (errors, patchFile) = doJsonDiff(sampleJson, currentSampleJson)

          (if (structureDiff.length <= 20) {
             Console.printLine(
               s"""|==================== Golden Json Change ====================
                   |${patchFilePath}
                   |==================== Structure Diff ==================
                   |${structureDiff}
                   |==================== Description ==================
                   |${errors}
                   |""".stripMargin
             )
           } else {
             Console.printLine(
               s"""|==================== Golden Json Change ====================
                   |${patchFilePath}
                   |==================== Description ==================
                   |${errors}
                   |""".stripMargin
             )
           }) *> {
            val cmd =
              Command(
                "git",
                "diff",
                "-U5",
                "--no-index",
                filePath.toString,
                diffFilePath.toString
              )
            cmd.lines
              .map { linesChunk =>
                if (linesChunk.filterNot(_ == "").isEmpty) {
                  ???
                }

                linesChunk.map { line =>
                  line
                    .replace(diffFilePath.toString, filePath.toString)
                    .replace(Paths.get("").toAbsolutePath().toString, "")
                }
              }
              .flatMap { lines =>
                writeContentToFile(patchFilePath, lines.mkString("\n") + "\n")
              }
          } *>
            writeSampleToFile(diffFilePath, sample) *>
            // ZIO.succeed(assertTrue(sample == currentSample))
            ZIO.succeed(assertTrue(false))
        }
    } yield assertion
  }

  private def createNewTest[A: JsonEncoder](
      resourceDir: Path,
      name: String,
      gen: Gen[Sized, A],
      sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Throwable, TestResult] = {
    val fileName = s"${name}_new.json"
    val filePath = resourceDir.resolve(fileName)

    val failureString =
      s"No existing golden test for ${resourceDir.resolve(s"$name.json")}. Remove _new from the suffix and re-run the test."

    for {
      sample <- generateSample(gen, sampleSize)
      _ <-
        ZIO
          .ifZIO(ZIO.attemptBlocking(Files.exists(filePath)))(ZIO.unit, ZIO.attemptBlocking(Files.createFile(filePath)))
      _ <- writeSampleToFile(filePath, sample)
      assertion = TestArrow.make((_: Any) => TestTrace.fail(failureString).withLocation(Some(trace.toString)))
    } yield TestResult(assertion)
  }

  private def generateSample[A: JsonEncoder](
      gen: Gen[Sized, A],
      sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Exception, GoldenSample] =
    Gen
      .listOfN(sampleSize)(gen)
      .sample
      .map(_.value)
      .map { elements =>
        val jsonElements = elements.map(_.toJsonAST).collect { case Right(a) => a }
        val jsonArray    = new Json.Arr(Chunk.fromIterable(jsonElements))
        GoldenSample(jsonArray)
      }
      .runHead
      .someOrFailException

  private def getName[A](implicit tag: Tag[A]): String =
    tag.tag.shortName

}

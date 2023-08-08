package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.runtime.MorphirRuntime
import scala.io.Source
import zio.json.*
import zio.*
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}

trait EvaluationLibraryPlatformSpecific {
  def apply(fileName: String, prefix: Option[String] = None): EvaluationLibrary = {
    val text = Source
      .fromFile(fileName)
      .getLines()
      .mkString("\n")
    val morphirIRFile = text.fromJson[MorphirIRFile]
    val distribution = morphirIRFile
      .getOrElse(throw new Exception(morphirIRFile.toString))
      .distribution
//      .asInstanceOf[Library]
//    val store = Store.fromLibrary(library)
    EvaluationLibrary(MorphirRuntime.quick(distribution), prefix)
  }

  def apply(fileName: String, prefix: String): EvaluationLibrary = apply(fileName, Some(prefix))

  def loadDistribution(fileName: String): Distribution = {
    val text = Source
      .fromFile(fileName)
      .getLines()
      .mkString("\n")
    val morphirIRFile = text.fromJson[MorphirIRFile]
    morphirIRFile
      .getOrElse(throw new Exception(s"Failed to load $fileName as distribution"))
      .distribution
  }

  def loadDistributionFromFileZIO(fileName: String): Task[Distribution] =
    for {
      fileContents <- ZIO.readFile(fileName)
      morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
        .mapError(MorphirIRDecodingError(_))
    } yield morphirIRFile.distribution

}

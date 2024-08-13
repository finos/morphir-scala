package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.{Value => V}
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.runtime.MorphirRuntime
import scala.io.Source
import zio.json.*
import zio.*
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}
import org.finos.morphir.runtime.MorphirRuntimeError.*

trait EvaluationLibraryPlatformSpecific {

  def loadDistributionUnsafe(fileName: String): Distribution =
    Unsafe.unsafe {
      implicit unsafe =>
        Runtime.default.unsafe.run(loadDistributionFromFileZIO(fileName)).getOrThrowFiberFailure()
    }

  def loadDistributionFromFileZIO(fileName: String): Task[Distribution] =
    for {
      fileContents <- ZIO.readFile(fileName)
      morphirIRFile <- ZIO.fromEither(fileContents.fromJson[MorphirIRFile])
        .mapError(MorphirIRDecodingError(_))
    } yield morphirIRFile.distribution

  def loadValueFromFileZIO(fileName: String): Task[TypedValue] =
    for {
      fileContents <- ZIO.readFile(fileName)
      value <- ZIO.fromEither(fileContents.fromJson[TypedValue])
        .mapError(MorphirIRDecodingError(_))
    } yield value
}

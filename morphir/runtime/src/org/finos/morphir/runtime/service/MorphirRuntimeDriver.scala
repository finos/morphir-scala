package org.finos.morphir.runtime.service

import org.finos.morphir.datamodel.Data
import org.finos.morphir.naming
import zio._
import org.finos.morphir.naming._
import org.finos.morphir.runtime.config.MorphirRuntimeConfig
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.{ConfigurationError, MorphirRuntime, MorphirRuntimeError, RTExecutionContext}

trait MorphirRuntimeDriver {

  def evaluate(entryPoint: FQName, param: Data, params: Data*): Task[Data]

  def test(): Task[Unit]
}

object MorphirRuntimeDriver extends MorphirRuntimeDriverPlatformSpecific {

  def evaluate(entryPoint: FQName, param: Data, params: Data*): ZIO[MorphirRuntimeDriver, Throwable, Data] =
    ZIO.serviceWithZIO[MorphirRuntimeDriver](_.evaluate(entryPoint, param, params: _*))

  def test(): ZIO[MorphirRuntimeDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirRuntimeDriver](_.test())
}

final case class MorphirRuntimeDriverDefault(
    runtime: MorphirRuntime,
    context: RTExecutionContext,
    morphirEnv: ZEnvironment[MorphirEnv],
    dumper: MorphirRuntimeDiagnostics
) extends MorphirRuntimeDriver {

  override def evaluate(entryPoint: naming.FQName, param: Data, params: Data*): Task[Data] =
    for {
      _ <- ZIO.logTrace(s"Executing MorphirRuntimeDriver.evaluate(${entryPoint}, ${param}, ${params}")
      runtimeConfig <- ZIO.config[MorphirRuntimeConfig](MorphirRuntimeConfig.config).mapError(err =>
        ConfigurationError("Issue encountered while getting MorphirRuntimeConfig", Option(err))
      )
      _ <- ZIO.when(runtimeConfig.diagnosticsOutputDir.isDefined) {
        val paramsFinal = param +: params
        ZIO.foreach(paramsFinal) { (param: Data) =>
          for {
            text <- dumper.dataToText(param, "json")
            // TODO: Change this to writing a file to disk
            _ <- Console.printLine(text)
          } yield ()
        }
      }
      resultData <- runtime.evaluate(entryPoint, param, params: _*)
        .provideEnvironment(morphirEnv)
        .provideState(context)
        .toZIO
    } yield resultData

  override def test(): Task[Unit] =
    for {
      _ <- Console.printLine("MorphirRuntimeDriver test command executing")
      _ <- Console.printLine("MorphirRuntimeDriver test command executed")
    } yield ()
}



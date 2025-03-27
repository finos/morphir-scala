package org.finos.morphir.runtime.service

import org.finos.morphir.datamodel.Data
import org.finos.morphir.naming
import zio._
import org.finos.morphir.naming._
import org.finos.morphir.runtime.config.MorphirRuntimeConfig
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.{ConfigurationError, MorphirRuntime, MorphirRuntimeError, RTExecutionContext}

trait MorphirRuntimeDiagnostics {
  def dataToText(data: Data, format: String): Task[String]
  def writeEvaluationInputsTo(targetDirectory: String)(entryPoint: FQName, param: Data, params: Data*): Task[Unit]
}

package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.UType

case class EvaluationLibrary(runtime: MorphirRuntime[scala.Unit, UType], modulePrefix: Option[String])

//TODO: Delete after moving loadDistribution to json support package
object EvaluationLibrary extends EvaluationLibraryPlatformSpecific {}

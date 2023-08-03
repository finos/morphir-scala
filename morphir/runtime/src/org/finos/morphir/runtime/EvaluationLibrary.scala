package org.finos.morphir.runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.runtime.quick.{EvaluatorQuick, QuickMorphirRuntime, Store}
import org.finos.morphir.ir.Distribution.Distribution.Library
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Type.UType

case class EvaluationLibrary(runtime: MorphirRuntime[scala.Unit, UType], modulePrefix: Option[String])

//TODO: Delete after moving loadDistribution to json support package
object EvaluationLibrary extends EvaluationLibraryPlatformSpecific {}

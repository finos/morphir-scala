package org.finos.morphir.runtime.internal

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.TypedMorphirRuntimeDefs.ValueAttribs

trait InvokeableEvaluator {
  def handleApplyResult2(
      va: ValueAttribs,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue
  ): RTValue

  def handleApplyResult(
      va: ValueAttribs,
      functionValue: RTValue,
      argValue: RTValue
  ): RTValue
}

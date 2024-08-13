package org.finos.morphir.runtime.internal

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.ir.Type.UType

trait InvokeableEvaluator {

  def handleApplyResult5(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue,
      arg4: RTValue,
      arg5: RTValue
  ): RTValue

  def handleApplyResult4(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue,
      arg4: RTValue
  ): RTValue

  def handleApplyResult3(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue
  ): RTValue

  def handleApplyResult2(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue
  ): RTValue

  def handleApplyResult(
      va: UType,
      functionValue: RTValue,
      argValue: RTValue
  ): RTValue
}

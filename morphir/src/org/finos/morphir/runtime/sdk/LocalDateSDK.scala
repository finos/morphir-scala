package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.MorphirRuntimeError.UnexpectedType
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.{RTValue => RT}
import org.finos.morphir.runtime.internal.*

object LocalDateSDK {
  extension (rtLd: RT.LocalDate)
    def update(f: java.time.LocalDate => java.time.LocalDate): RT.LocalDate =
      rtLd.copy(value = f(rtLd.value))

  val addWeeks = DynamicNativeFunction2("addWeeks") {
    (_: NativeContext) => (weeksArg: RT.Primitive.Int, localDateArg: RT.LocalDate) =>
      localDateArg.update(_.plusWeeks(weeksArg.value.toInt))
  }

  val diffInDays = DynamicNativeFunction2("diffInDays") {
    (_: NativeContext) => (localDateArg1: RT.LocalDate, localDateArg2: RT.LocalDate) =>
      {
        val ld1        = localDateArg1.value
        val ld2        = localDateArg2.value
        val diffInDays = (ld2.toEpochDay - ld1.toEpochDay).toInt
        RT.Primitive.Int(diffInDays)
      }
  }
}

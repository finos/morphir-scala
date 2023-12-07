package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.*

import java.time.LocalTime as JLocalTime

object LocalTimeSDK {
  extension (rtLt: RTValue.LocalTime)
    def update(f: JLocalTime => JLocalTime): RTValue.LocalTime =
      rtLt.copy(value = f(rtLt.value))

  val addHours = DynamicNativeFunction2("addHours") {
    (_: NativeContext) => (hoursArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
      localTimeArg.update(_.plusHours(hoursArg.value.toInt))
  }

  val addMinutes = DynamicNativeFunction2("addMinutes") {
    (_: NativeContext) =>
      (minutesArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
        localTimeArg.update(_.plusMinutes(minutesArg.value.toInt))
  }
}

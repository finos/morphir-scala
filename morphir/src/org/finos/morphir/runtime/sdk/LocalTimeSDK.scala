package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.*

import java.time.LocalTime as JLocalTime
import java.time.format.DateTimeFormatter as JDateTimeFormatter
import scala.util.control.NonFatal

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
  val addSeconds = DynamicNativeFunction2("addSeconds") {
    (_: NativeContext) =>
      (secondsArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
        localTimeArg.update(_.plusSeconds(secondsArg.value.toInt))
  }

  val diffInSeconds = DynamicNativeFunction2("diffInSeconds") {
    (_: NativeContext) =>
      (localTimeArg1: RTValue.LocalTime, localTimeArg2: RTValue.LocalTime) => {
        val lt1 = localTimeArg1.value
        val lt2 = localTimeArg2.value
        // NOTE: this behavior (a - b) is the opposite of LocalDate's diffIn* (b - a)
        //       in order to conform to the morphir-elm SDK implementation exactly.
        val diffInSeconds = lt1.toSecondOfDay - lt2.toSecondOfDay
        RTValue.Primitive.Int(diffInSeconds)
      }
  }

  val fromISO = DynamicNativeFunction1("fromISO") {
    (_: NativeContext) =>
      (isoArg: RTValue.Primitive.String) => {
        val maybeLocalTime =
          try Some(JLocalTime.parse(isoArg.value, JDateTimeFormatter.ISO_LOCAL_TIME))
          catch { case NonFatal(_) => None }
        val maybeLocalTimeRT = maybeLocalTime.map(RTValue.LocalTime.apply)
        MaybeSDK.resultToMaybe(maybeLocalTimeRT)
      }
  }
}

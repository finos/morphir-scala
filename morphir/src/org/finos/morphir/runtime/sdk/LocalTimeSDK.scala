package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.*

import java.time.LocalTime as JLocalTime
import java.time.format.DateTimeFormatter as JDateTimeFormatter
import scala.util.control.NonFatal

object LocalTimeSDK {
  def update(rtLt: RTValue.LocalTime)(f: JLocalTime => JLocalTime): RTValue.LocalTime =
    rtLt.copy(value = f(rtLt.value))

  val addHours = DynamicNativeFunction2("addHours") {
    (_: NativeContext) => (hoursArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
      update(localTimeArg)(_.plusHours(hoursArg.value.toLong))
  }

  val addMinutes = DynamicNativeFunction2("addMinutes") {
    (_: NativeContext) => (minutesArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
      update(localTimeArg)(_.plusMinutes(minutesArg.value.toLong))
  }
  val addSeconds = DynamicNativeFunction2("addSeconds") {
    (_: NativeContext) => (secondsArg: RTValue.Primitive.Int, localTimeArg: RTValue.LocalTime) =>
      update(localTimeArg)(_.plusSeconds(secondsArg.value.toLong))
  }

  val diffInSeconds = DynamicNativeFunction2("diffInSeconds") {
    (_: NativeContext) => (localTimeArg1: RTValue.LocalTime, localTimeArg2: RTValue.LocalTime) =>
      val lt1 = localTimeArg1.value
      val lt2 = localTimeArg2.value
      // NOTE: this behavior (a - b) is the opposite of LocalDate's diffIn* (b - a)
      //       in order to conform to the morphir-elm SDK implementation exactly.
      val diffInSeconds = lt1.toSecondOfDay - lt2.toSecondOfDay
      RTValue.Primitive.Int(diffInSeconds)
  }

  val fromISO = DynamicNativeFunction1("fromISO") {
    (_: NativeContext) => (isoArg: RTValue.Primitive.String) =>
      val maybeLocalTime =
        try Some(JLocalTime.parse(isoArg.value, JDateTimeFormatter.ISO_LOCAL_TIME))
        catch { case NonFatal(_) => None }
      val maybeLocalTimeRT = maybeLocalTime.map(RTValue.LocalTime.apply)
      MaybeSDK.optionToMaybe(maybeLocalTimeRT)
  }

  val diffInHours = DynamicNativeFunction2("diffInHours") {
    (_: NativeContext) => (localTimeArg1: RTValue.LocalTime, localTimeArg2: RTValue.LocalTime) =>
      val lt1 = localTimeArg1.value
      val lt2 = localTimeArg2.value
      // NOTE: this behavior (a - b) is the opposite of LocalDate's diffIn* (b - a)
      //       in order to conform to the morphir-elm SDK implementation exactly.
      val diffInHours = lt1.toSecondOfDay / 3600 - lt2.toSecondOfDay / 3600
      RTValue.Primitive.Int(diffInHours)
  }

  val diffInMinutes = DynamicNativeFunction2("diffInMinutes") {
    (_: NativeContext) => (localTimeArg1: RTValue.LocalTime, localTimeArg2: RTValue.LocalTime) =>
      val lt1 = localTimeArg1.value
      val lt2 = localTimeArg2.value
      // NOTE: this behavior (a - b) is the opposite of LocalDate's diffIn* (b - a)
      //       in order to conform to the morphir-elm SDK implementation exactly.
      val diffInMinutes = lt1.toSecondOfDay / 60 - lt2.toSecondOfDay / 60
      RTValue.Primitive.Int(diffInMinutes)
  }
}

package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.*

import java.time.LocalDate as JLocalDate
import java.time.format.DateTimeFormatter as JDateTimeFormatter
import scala.util.control.NonFatal

object LocalDateSDK {
  extension (rtLd: RTValue.LocalDate)
    def update(f: JLocalDate => JLocalDate): RTValue.LocalDate =
      rtLd.copy(value = f(rtLd.value))

  val addWeeks = DynamicNativeFunction2("addWeeks") {
    (_: NativeContext) => (weeksArg: RTValue.Primitive.Int, localDateArg: RTValue.LocalDate) =>
      localDateArg.update(_.plusWeeks(weeksArg.value.toInt))
  }

  val diffInDays = DynamicNativeFunction2("diffInDays") {
    (_: NativeContext) => (localDateArg1: RTValue.LocalDate, localDateArg2: RTValue.LocalDate) =>
      {
        val ld1        = localDateArg1.value
        val ld2        = localDateArg2.value
        val diffInDays = (ld2.toEpochDay - ld1.toEpochDay).toInt
        RTValue.Primitive.Int(diffInDays)
      }
  }

  val fromISO = DynamicNativeFunction1("fromISO") {
    // The Elm date library underlying the Morphir SDK parses ISO dates that
    // are split into three different formatters in the java library,
    // ISO_LOCAL_DATE, ISO_WEEK_DATE, and ISO_ORDINAL_DATE.  For compatibility,
    // we try all three formatters.
    // https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromIsoString
    // https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html

    def maybeParse(formatter: JDateTimeFormatter, s: String): Option[JLocalDate] =
      try Some(JLocalDate.parse(s, formatter))
      catch { case NonFatal(_) => None }

    def elmCompatIsoParse(isoStr: String): Option[JLocalDate] =
      maybeParse(JDateTimeFormatter.ISO_LOCAL_DATE, isoStr)
        .orElse(maybeParse(JDateTimeFormatter.ISO_WEEK_DATE, isoStr))
        .orElse(maybeParse(JDateTimeFormatter.ISO_ORDINAL_DATE, isoStr))

    (_: NativeContext) =>
      (isoArg: RTValue.Primitive.String) => {
        val isoStr           = isoArg.value
        val maybeLocalDate   = elmCompatIsoParse(isoStr)
        val maybeLocalDateRT = maybeLocalDate.map(RTValue.LocalDate.apply)
        MaybeSDK.resultToMaybe(maybeLocalDateRT)
      }
  }
}

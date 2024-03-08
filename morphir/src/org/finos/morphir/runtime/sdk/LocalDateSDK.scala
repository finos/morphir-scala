package org.finos.morphir.runtime.sdk

import org.finos.morphir.naming.FQName
import org.finos.morphir.runtime.ErrorUtils.tryOption
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.*

import java.time.{
  DayOfWeek as JDayOfWeek,
  LocalDate as JLocalDate,
  Month as JMonth,
  Year as JYear,
  YearMonth as JYearMonth
}
import java.time.format.DateTimeFormatter as JDateTimeFormatter
import scala.util.control.NonFatal

object LocalDateSDK {
  private def update(rtLd: RTValue.LocalDate)(f: JLocalDate => JLocalDate): RTValue.LocalDate =
    rtLd.copy(value = f(rtLd.value))

  private def clamp(min: Int, max: Int)(value: Int): Int = Math.max(min, Math.min(max, value))

  val fromOrdinalDate = DynamicNativeFunction2("fromOrdinalDate") {
    (_: NativeContext) => (yearArg: RTValue.Primitive.Int, dayOfYearArg: RTValue.Primitive.Int) =>
      {
        val year      = yearArg.value.toInt
        val dayOfYear = dayOfYearArg.value.toInt

        val clampedYear = clamp(JYear.MIN_VALUE, JYear.MAX_VALUE)(year)

        val minDay           = 1
        val maxDay           = if (JYear.of(clampedYear).isLeap) 366 else 365
        val clampedDayOfYear = clamp(minDay, maxDay)(dayOfYear)

        RTValue.LocalDate(JLocalDate.ofYearDay(clampedYear, clampedDayOfYear))
      }
  }

  val fromParts = DynamicNativeFunction3("fromParts") {
    (_: NativeContext) =>
      (yearArg: RTValue.Primitive.Int, monthArg: RTValue.Primitive.Int, dayArg: RTValue.Primitive.Int) =>
        {
          val year  = yearArg.value.toInt
          val month = monthArg.value.toInt
          val day   = dayArg.value.toInt

          val maybeLocalDate   = tryOption(java.time.LocalDate.of(year, month, day))
          val maybeLocalDateRt = maybeLocalDate.map(RTValue.LocalDate.apply)

          MaybeSDK.resultToMaybe(maybeLocalDateRt)
        }
  }

  val fromCalendarDate = DynamicNativeFunction3("fromCalendarDate") {
    (_: NativeContext) => (yearArg: RTValue.Primitive.Int, monthArg: RTValue, dayArg: RTValue.Primitive.Int) =>
      {
        val year: Int  = yearArg.value.toInt
        val month: Int = RTValue.Month.coerceJavaMonth(monthArg).getValue
        val day: Int   = dayArg.value.toInt

        val clampedYear  = clamp(JYear.MIN_VALUE, JYear.MAX_VALUE)(year)
        val clampedMonth = clamp(1, 12)(month)

        val clampedYearMonth = JYearMonth.of(clampedYear, month)
        val minDay           = 1
        val maxDay           = clampedYearMonth.atEndOfMonth.getDayOfMonth
        val clampedDay       = clamp(minDay, maxDay)(day)

        RTValue.LocalDate(JLocalDate.of(clampedYear, month, clampedDay))
      }
  }

  val addDays = DynamicNativeFunction2("addDays") {
    (_: NativeContext) => (weeksArg: RTValue.Primitive.Int, localDateArg: RTValue.LocalDate) =>
      update(localDateArg)(_.plusDays(weeksArg.value.toLong))
  }

  val addWeeks = DynamicNativeFunction2("addWeeks") {
    (_: NativeContext) => (weeksArg: RTValue.Primitive.Int, localDateArg: RTValue.LocalDate) =>
      update(localDateArg)(_.plusWeeks(weeksArg.value.toLong))
  }

  val addYears = DynamicNativeFunction2("addYears") {
    (_: NativeContext) => (weeksArg: RTValue.Primitive.Int, localDateArg: RTValue.LocalDate) =>
      update(localDateArg)(_.plusYears(weeksArg.value.toLong))
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
      tryOption(JLocalDate.parse(s, formatter))

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

  val year = DynamicNativeFunction1("year") {
    (_: NativeContext) => (localDate: RTValue.LocalDate) => RTValue.Primitive.Int(localDate.value.getYear)
  }

  val month = DynamicNativeFunction1("month") {
    (_: NativeContext) => (localDate: RTValue.LocalDate) => RTValue.Month.fromJavaMonth(localDate.value.getMonth)
  }

  val monthNumber = DynamicNativeFunction1("monthNumber") {
    (_: NativeContext) => (localDate: RTValue.LocalDate) => RTValue.Primitive.Int(localDate.value.getMonth.getValue)
  }

  val day = DynamicNativeFunction1("day") {
    (_: NativeContext) => (localDate: RTValue.LocalDate) => RTValue.Primitive.Int(localDate.value.getDayOfMonth)
  }

  val dayOfWeek = DynamicNativeFunction1("dayOfWeek") {
    (_: NativeContext) => (localDate: RTValue.LocalDate) =>
      RTValue.DayOfWeek.fromJavaDayOfWeek(localDate.value.getDayOfWeek)
  }
}

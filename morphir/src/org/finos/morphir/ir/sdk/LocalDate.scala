package org.finos.morphir.ir.sdk

import zio.Chunk
import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Specification.{CustomTypeSpecification, OpaqueTypeSpecification}
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics.intType
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.ir.sdk.String.stringType

object LocalDate extends MorphirIRSdkModule("LocalDate") {
  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("LocalDate") -> OpaqueTypeSpecification() ?? "Type that represents a date concept.",
      name("Month")
        -> CustomTypeSpecification(
          Chunk.empty,
          UConstructors(
            Map(
              name("January")   -> Chunk.empty,
              name("February")  -> Chunk.empty,
              name("March")     -> Chunk.empty,
              name("April")     -> Chunk.empty,
              name("May")       -> Chunk.empty,
              name("June")      -> Chunk.empty,
              name("July")      -> Chunk.empty,
              name("August")    -> Chunk.empty,
              name("September") -> Chunk.empty,
              name("October")   -> Chunk.empty,
              name("November")  -> Chunk.empty,
              name("December")  -> Chunk.empty
            )
          )
        )
        ?? "Type that represents an month concept.",
      name("DayOfWeek")
        -> CustomTypeSpecification(
          Chunk.empty,
          UConstructors(
            Map(
              name("Monday")    -> Chunk.empty,
              name("Tuesday")   -> Chunk.empty,
              name("Wednesday") -> Chunk.empty,
              name("Thursday")  -> Chunk.empty,
              name("Friday")    -> Chunk.empty,
              name("Saturday")  -> Chunk.empty,
              name("Sunday")    -> Chunk.empty
            )
          )
        )
        ?? "Type that represents a day of week concept."
    ),
    values = Map(
      vSpec("fromISO", "iso" -> stringType)(maybeType(localDateType)),
      vSpec("fromParts", "year" -> intType, "month" -> intType, "day" -> intType)(maybeType(localDateType)),
      vSpec("diffInDays", "date1" -> localDateType, "date2" -> localDateType)(intType),
      vSpec("diffInWeeks", "date1" -> localDateType, "date2" -> localDateType)(intType),
      vSpec("diffInMonths", "date1" -> localDateType, "date2" -> localDateType)(intType),
      vSpec("diffInYears", "date1" -> localDateType, "date2" -> localDateType)(intType),
      vSpec("addDays", "offset" -> intType, "startDate" -> localDateType)(localDateType),
      vSpec("addWeeks", "offset" -> intType, "startDate" -> localDateType)(localDateType),
      vSpec("addMonths", "offset" -> intType, "startDate" -> localDateType)(localDateType),
      vSpec("addYears", "offset" -> intType, "startDate" -> localDateType)(localDateType)
    )
  )

  lazy val localDateType: UType = reference(fqn("LocalDate"))
  def localDateType[A](attributes: A): Type[A] =
    reference(attributes, fqn("LocalDate"))

  lazy val monthType: UType                = reference(fqn("Month"))
  def monthType[A](attributes: A): Type[A] = reference(attributes, fqn("Month"))

  lazy val dayOfWeekType: UType                = reference(fqn("DayOfWeek"))
  def dayOfWeekType[A](attributes: A): Type[A] = reference(attributes, fqn("DayOfWeek"))
}

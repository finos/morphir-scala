package org.finos.morphir.mir.sdk

import org.finos.morphir.mir.Module
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.mir.Type.Type._
import org.finos.morphir.mir.Type.{Type, UType}
import org.finos.morphir.mir.sdk.Basics.intType
import org.finos.morphir.mir.sdk.Common._
import org.finos.morphir.mir.sdk.Maybe.maybeType
import org.finos.morphir.mir.sdk.String.stringType
import org.finos.morphir.syntax.NamingSyntax._

object LocalDate {
  val moduleName: ModuleName = ModuleName.fromString("LocalDate")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("LocalDate") -> OpaqueTypeSpecification() ?? "Type that represents a date concept."),
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

  lazy val localDateType: UType = reference(toFQName(moduleName, "LocalDate"))
  def localDateType[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "LocalDate"))
}

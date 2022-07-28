package morphir.ir.sdk

import morphir.ir.Module
import morphir.ir.Module.ModuleName
import morphir.ir.Type.Specification.OpaqueTypeSpecification
import morphir.ir.Type.Type._
import morphir.ir.Type.{Type, UType}
import morphir.ir.sdk.Basics.intType
import morphir.ir.sdk.Common._
import morphir.ir.sdk.Maybe.maybeType
import morphir.ir.sdk.String.stringType
import morphir.syntax.NamingSyntax._

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

package morphir.mir.sdk

import morphir.mir.Module
import morphir.mir.Module.ModuleName
import morphir.mir.Type.Specification.OpaqueTypeSpecification
import morphir.mir.Type.Type._
import morphir.mir.Type.{Type, UType}
import morphir.mir.sdk.Basics.intType
import morphir.mir.sdk.Common._
import morphir.mir.sdk.Maybe.maybeType
import morphir.mir.sdk.String.stringType
import morphir.syntax.NamingSyntax._

object LocalTime {
  val moduleName: ModuleName = ModuleName.fromString("LocalTime")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("LocalTime") -> OpaqueTypeSpecification() ?? "Type that represents a time concept."),
    values = Map(
      vSpec("fromISO", "iso" -> stringType)(maybeType(localTimeType)),
      vSpec("fromMilliseconds", "millis" -> intType)(localTimeType),
      vSpec("diffInSeconds", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("diffInMinutes", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("diffInHours", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("addSeconds", "seconds" -> intType, "time" -> localTimeType)(localTimeType),
      vSpec("addMinutes", "minutes" -> intType, "time" -> localTimeType)(localTimeType),
      vSpec("addHours", "hours" -> intType, "time" -> localTimeType)(localTimeType)
    )
  )

  lazy val localTimeType: UType = reference(toFQName(moduleName, "localTime"))
  def localTimeType[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "localTime"))
}

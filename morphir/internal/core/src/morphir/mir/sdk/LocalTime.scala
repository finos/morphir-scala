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

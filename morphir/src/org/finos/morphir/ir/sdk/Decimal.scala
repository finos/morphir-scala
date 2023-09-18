package org.finos.morphir.ir.sdk

import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics._
import org.finos.morphir.ir.sdk.Common._
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.ir.sdk.String.stringType
import org.finos.morphir.syntax.NamingSyntax._

object Decimal extends MorphirIRSdkModule("Decimal") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Decimal") -> OpaqueTypeSpecification() ?? "Type that represents a Decimal."
    ),
    values = Map(
      vSpec("fromInt", "n" -> intType)(decimalType),
      vSpec("fromFloat", "f" -> floatType)(decimalType),
      vSpec("fromString", "str" -> stringType)(maybeType(decimalType)),
      vSpec("hundred", "n" -> intType)(decimalType),
      vSpec("thousand", "n" -> intType)(decimalType),
      vSpec("million", "n" -> intType)(decimalType),
      vSpec("tenth", "n" -> intType)(decimalType),
      vSpec("hundredth", "n" -> intType)(decimalType),
      vSpec("millionth", "n" -> intType)(decimalType),
      vSpec("bps", "n" -> intType)(decimalType),
      vSpec("toString", "decimalValue" -> decimalType)(stringType),
      vSpec("toFloat", "d" -> decimalType)(floatType),
      vSpec("add", "a" -> decimalType, "b" -> decimalType)(decimalType),
      vSpec("sub", "a" -> decimalType, "b" -> decimalType)(decimalType),
      vSpec("negate", "value" -> decimalType)(decimalType),
      vSpec("mul", "a" -> decimalType, "b" -> decimalType)(decimalType),
      vSpec("div", "a" -> decimalType, "b" -> decimalType)(maybeType(decimalType)),
      vSpec("divWithDefault", "default" -> decimalType, "a" -> decimalType, "b" -> decimalType)(maybeType(decimalType)),
      vSpec("truncate", "d" -> decimalType)(decimalType),
      vSpec("round", "d" -> decimalType)(decimalType),
      vSpec("gt", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("gte", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("eq", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("neq", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("lt", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("lte", "a" -> decimalType, "b" -> decimalType)(boolType),
      vSpec("compare", "a" -> decimalType, "b" -> decimalType)(orderType),
      vSpec("abs", "value" -> decimalType)(decimalType),
      vSpec("shiftDecimalLeft", "n" -> intType, "value" -> decimalType)(decimalType),
      vSpec("shiftDecimalRight", "n" -> intType, "value" -> decimalType)(decimalType),
      vSpec("zero")(decimalType),
      vSpec("one")(decimalType),
      vSpec("minusOne")(decimalType)
    )
  )

  lazy val decimalType: UType =
    reference(toFQName("Decimal"))
  def decimalType[A](attributes: A): Type[A] =
    reference(attributes, fqn("Decimal"))

  lazy val roundingModeType: UType =
    reference(toFQName("RoundingMode"))
  def roundingModeType[A](attributes: A): Type[A] =
    reference(attributes, fqn("RoundingMode"))
}

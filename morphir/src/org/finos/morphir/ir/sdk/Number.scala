package org.finos.morphir.ir.sdk

import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics.{boolType, intType}
import org.finos.morphir.ir.sdk.Decimal.decimalType
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.ir.sdk.Result.resultType
import org.finos.morphir.ir.sdk.String.stringType

object Number extends MorphirIRSdkModule("Number") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("Decimal") -> OpaqueTypeSpecification() ?? "Type that represents a Decimal."),
    values = Map(
      vSpec("fromInt", "n" -> intType)(numberType),
      vSpec("equal", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("notEqual", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("lessThan", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("lessThanOrEqual", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("greaterThan", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("greaterThanOrEqual", "a" -> numberType, "b" -> numberType)(boolType),
      vSpec("add", "a" -> numberType, "b" -> numberType)(numberType),
      vSpec("subtract", "a" -> numberType, "b" -> numberType)(numberType),
      vSpec("multiply", "a" -> numberType, "b" -> numberType)(numberType),
      vSpec("divide", "a" -> numberType, "b" -> numberType)(resultType(divisionByZeroType, numberType)),
      vSpec("abs", "value" -> numberType)(numberType),
      vSpec("negate", "value" -> numberType)(numberType),
      vSpec("reciprocal", "value" -> numberType)(numberType),
      vSpec("coerceToDecimal", "default" -> decimalType, "number" -> numberType)(decimalType),
      vSpec("toDecimal", "number" -> numberType)(maybeType(decimalType)),
      vSpec("toFractionalString", "num" -> numberType)(stringType),
      vSpec("simplify", "value" -> numberType)(maybeType(numberType)),
      vSpec("isSimplified", "a" -> numberType)(boolType),
      vSpec("zero")(numberType),
      vSpec("one")(numberType)
    )
  )

  lazy val numberType: UType                = reference(fqn("Number"))
  def numberType[A](attributes: A): Type[A] = reference(attributes, fqn("Number"))

  lazy val divisionByZeroType: UType                = reference(fqn("DivisionByZero"))
  def divisionByZeroType[A](attributes: A): Type[A] = reference(attributes, fqn("DivisionByZero"))

}

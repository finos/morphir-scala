package morphir.mir.sdk

import morphir.mir.Module
import morphir.mir.Module.ModuleName
import morphir.mir.Type.Specification.OpaqueTypeSpecification
import morphir.mir.Type.Type._
import morphir.mir.Type.{Type, UType}
import morphir.mir.sdk.Basics.{boolType, intType}
import morphir.mir.sdk.Common._
import morphir.mir.sdk.Decimal.decimalType
import morphir.mir.sdk.Maybe.maybeType
import morphir.mir.sdk.Result.resultType
import morphir.mir.sdk.String.stringType
import morphir.syntax.NamingSyntax._

object Number {
  val moduleName: ModuleName = ModuleName.fromString("Number")

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

  lazy val numberType: UType                = reference(toFQName(moduleName, "Number"))
  def numberType[A](attributes: A): Type[A] = reference(attributes, toFQName(moduleName, "Number"))

  lazy val divisionByZeroType: UType                = reference(toFQName(moduleName, "DivisionByZero"))
  def divisionByZeroType[A](attributes: A): Type[A] = reference(attributes, toFQName(moduleName, "DivisionByZero"))

}

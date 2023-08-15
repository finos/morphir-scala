package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import zio.Chunk

abstract class MorphirTypeError(msg: String) extends Exception(msg) {
  def getMsg: String = msg
}
object MorphirTypeError {
  def succinct[TA, VA](value: Value[TA, VA]): String = Succinct.Value(value)
  def succinct[TA](tpe: Type[TA]): String            = Succinct.Type(tpe)

  def succinct[TA](spec: T.Specification[TA]): String = Succinct.TypeSpec(spec)

  case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
      extends MorphirTypeError(s"$msg: ${succinct(tpe1)} vs ${succinct(tpe2)}")

  case class ArgumentDoesNotMatchParameter(arg: TypedValue, param: UType) extends MorphirTypeError(
        s"Argument ${succinct(arg)}  of type ${succinct(arg.attributes)} does not match parameter ${succinct(param)}"
      )

  case class ApplyToNonFunction(nonFunction: TypedValue, arg: TypedValue) extends MorphirTypeError(
        s"Tried to apply ${succinct(arg)} to ${succinct(nonFunction)} of type ${succinct(nonFunction.attributes)}, which is not a function"
      )

  case class LiteralTypeMismatch(lit: Lit, tpe: UType)
      extends MorphirTypeError(s"Literal $lit is not of type ${succinct(tpe)}")
  case class ImproperType(tpe: UType, msg: String) extends MorphirTypeError(s"$msg. Found: ${succinct(tpe)}")

  case class ImproperTypeSpec(fqn: FQName, spec: UTypeSpec, msg: String)
      extends MorphirTypeError(s"$msg. $fqn points to: ${succinct(spec)}")
  case class CannotDealias(err: LookupError, msg: String = "Cannot dealias type")
      extends MorphirTypeError(s"$msg: ${err.getMsg}")
  case class TypeVariableMissing(name: Name)     extends MorphirTypeError(s"Missing type variable $name.toTitleCase")
  case class DefinitionMissing(err: LookupError) extends MorphirTypeError(s"Cannot find definition: ${err.getMsg}")
  case class TypeMissing(err: LookupError)       extends MorphirTypeError(s"Cannot find type: ${err.getMsg}")
  case class OtherTypeError(msg: String)         extends MorphirTypeError(msg)
  case class TypeLacksField(tpe: UType, field: Name, msg: String)
      extends MorphirTypeError(s"${succinct(tpe)} lacks field ${field.toCamelCase}. $msg")
  case class TypeHasExtraField(tpe: UType, contract: UType, field: Name) extends MorphirTypeError(
        s"${succinct(tpe)} has field ${field.toCamelCase}, which is not included in ${succinct(contract)}"
      )

  case class ValueLacksField(value: TypedValue, contract: UType, field: Name) extends MorphirTypeError(
        s"${succinct(value)} lacks field ${field.toCamelCase}, which is required by ${succinct(contract)}"
      )
  case class ValueHasExtraField(value: TypedValue, contract: UType, field: Name) extends MorphirTypeError(
        s"${succinct(value)} has field ${field.toCamelCase}, which is not included in ${succinct(contract)}"
      )
  case class TypeHasDifferentFieldType(first: UType, second: UType, field: Name, firstTpe: UType, secondTpe: UType)
      extends MorphirTypeError(
        s"tpe for field ${field.toCamelCase} is ${succinct(firstTpe)} in ${succinct(first)} but ${succinct(secondTpe)} in ${succinct(second)}"
      )
  case class ConstructorMissing(err: LookupError, tpe: UType)
      extends MorphirTypeError(s"Cannot find definition of type ${succinct(tpe)}: ${err.getMsg}")
  case class ModuleMissing(modName: ModuleName)   extends MorphirTypeError("Todo")
  case class PackageMissing(pckName: PackageName) extends MorphirTypeError("Todo")
  case class Unimplemented(s: String)             extends MorphirTypeError(s)
}

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
  case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
      extends MorphirTypeError(s"$msg: ${succinct(tpe1)} vs ${succinct(tpe2)}")
  case class ArgumentDoesNotMatchParameter(arg: TypedValue, param: UType) extends MorphirTypeError(
        s"Argument ${succinct(arg)}  of type ${succinct(arg.attributes)} does not match parameter ${succinct(param)}"
      )
  case class ApplyToNonFunction(nonFunction: TypedValue, arg: TypedValue) extends MorphirTypeError(
        s"Tried to apply ${succinct(arg)} to ${succinct(nonFunction)} of type ${succinct(nonFunction.attributes)}, which is not a function"
      )
  case class LiteralTypeMismatch(lit : Lit, tpe : UType) extends MorphirTypeError(s"Literal $lit is not of type ${succinct(tpe)}")
  case class ImproperType(tpe: UType, message: String) extends MorphirTypeError("Todo")
  case class CannotDealias(err: LookupError, msg: String = "Cannot dealias type")
      extends MorphirTypeError(s"$msg: ${err.getMsg}")
  case class TypeVariableMissing(name : Name) extends MorphirTypeError(s"Missing type variable $name.toTitleCase")
  case class ValueMissing(value: TypedValue)             extends MorphirTypeError("Todo")
  case class ConstructorMissing(fqn: FQName, tpe: UType) extends MorphirTypeError("Todo")
  case class ModuleMissing(modName: ModuleName)          extends MorphirTypeError("Todo")
  case class PackageMissing(pckName: PackageName)        extends MorphirTypeError("Todo")
  case class Unimplemented(s: String)                    extends MorphirTypeError(s)
}

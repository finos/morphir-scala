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
import zio.Chunk

abstract class MorphirTypeError(msg: String) extends Exception(msg){
  def getMsg : String = msg
}
object MorphirTypeError {
  def succinct(value : Value[TA, VA]) : String= Succinct.Value(value)
  def succinct(value: Vale[TA, VA]) : String = Succinct.Value(value)
  case class TypesMismatch(tpe1: UType, tpe2: UType, msg : String)                           extends MorphirTypeError(s"$msg: ${Succinct.Type(tp1)} vs ${Sucinct.Type(tpe2)}")
  case class ArgumentDoesNotMatchParameter(arg: TypedValue, param: UType) extends MorphirTypeError(s"Argument ${Succinct.Value(arg)}  of type ${Succinct.Type(arg.attributes)} does not match parameter ${Succinct.Type(param)}")
  case class ApplyToNonFunction(arg : TypedValue, nonFunction : TypedValue) extneds MorphirTypeErr(s"Tried to apply ${Succinct.}")
  case class ImproperType(tpe: UType, message: String)                         extends MorphirTypeError("Todo")
  case class CannotDealias(err : LookupError, msg : String = "Cannot dealias type")                                  extends MorphirTypeError(s"$msg: ${err.getMsg}")
  case class ValueMissing(value: TypedValue)                                   extends MorphirTypeError("Todo")
  case class ConstructorMissing(fqn: FQName, tpe: UType)                       extends MorphirTypeError("Todo")
  case class ModuleMissing(modName: ModuleName)                                extends MorphirTypeError("Todo")
  case class PackageMissing(pckName: PackageName)                              extends MorphirTypeError("Todo")
  case class Unimplemented(s: String)                                          extends MorphirTypeError(s)
}

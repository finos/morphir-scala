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

abstract class MorphirTypeError(msg: String) extends Exception(msg)
object MorphirTypeError {
  case class TypesMismatch(tpe1: UType, tpe2: UType)                           extends MorphirTypeError("Todo")
  case class ArgumentDoesNotMatchParameter(arg: TypedValue, param: TypedValue) extends MorphirTypeError("Todo")
  case class ImproperType(tpe: UType, message: String)                         extends MorphirTypeError("Todo")
  case class TypeMissing(tpe: UType)                                           extends MorphirTypeError("Todo")
  case class ValueMissing(value: TypedValue)                                   extends MorphirTypeError("Todo")
  case class ConstructorMissing(fqn: FQName, tpe: UType)                       extends MorphirTypeError("Todo")
  case class ModuleMissing(modName: ModuleName)                                extends MorphirTypeError("Todo")
  case class PackageMissing(pckName: PackageName)                              extends MorphirTypeError("Todo")
  case class Unimplemented(s: String)                                          extends MorphirTypeError(s)
}

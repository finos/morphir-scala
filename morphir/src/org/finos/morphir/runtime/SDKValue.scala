package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.{FieldT, UType}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value, TypedDefinition}
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.naming.*
import org.finos.morphir.naming.FQName.getLocalName
import org.finos.morphir.naming.Name.toTitleCase
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.internal.{NativeFunctionSignature, NativeFunctionSignatureAdv}
import org.finos.morphir.runtime.internal.NativeFunctionSignature.*
import zio.Chunk

sealed trait SDKValue
sealed trait SDKConstructor
object SDKConstructor {
  case class Implicit(fields: List[FieldT[Unit]]) extends SDKConstructor
  case class Explicit(arguments: List[UType])     extends SDKConstructor
}
object SDKValue {
  case class SDKValueDefinition(definition: TypedDefinition) extends SDKValue
  case class SDKNativeFunction(function: NativeFunctionSignature) extends SDKValue {
    def arguments = function.numArgs
  }

  object SDKNativeFunction {
    import NativeFunctionSignature.*
    def fun1(f: RTValue => RTValue) =
      new SDKNativeFunction(Fun1(f))
    def fun2(f: (RTValue, RTValue) => RTValue) =
      new SDKNativeFunction(Fun2(f))
    def fun3(f: (RTValue, RTValue, RTValue) => RTValue) =
      new SDKNativeFunction(Fun3(f))
    def fun4(f: (RTValue, RTValue, RTValue, RTValue) => RTValue) =
      new SDKNativeFunction(Fun4(f))
    def fun5(f: (
        RTValue,
        RTValue,
        RTValue,
        RTValue,
        RTValue
    ) => RTValue) =
      new SDKNativeFunction(Fun5(f))
    def fun6(f: (
        RTValue,
        RTValue,
        RTValue,
        RTValue,
        RTValue,
        RTValue
    ) => RTValue) =
      new SDKNativeFunction(Fun6(f))
  }

  case class SDKNativeInnerFunction(function: NativeFunctionSignatureAdv)
      extends SDKValue {
    def arguments = function.numArgs
  }

  case class SDKNativeValue(value: RTValue) extends SDKValue
}

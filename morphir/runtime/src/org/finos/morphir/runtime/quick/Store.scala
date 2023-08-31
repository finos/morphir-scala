package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.naming.FQName.getLocalName
import org.finos.morphir.naming.Name.toTitleCase
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.{Module, Type}
import zio.Chunk

sealed trait SDKValue[TA, VA]

case class SDKConstructor[TA, VA](arguments: List[VA])
object SDKValue {
  case class SDKValueDefinition[TA, VA](definition: Definition[TA, VA]) extends SDKValue[TA, VA]
  case class SDKNativeFunction[TA, VA](function: NativeFunctionSignature[TA, VA]) extends SDKValue[TA, VA] {
    def arguments = function.numArgs
  }

  object SDKNativeFunction {
    import NativeFunctionSignature._
    def fun1[TA, VA](f: Result[TA, VA] => Result[TA, VA]) =
      new SDKNativeFunction(Fun1[TA, VA](f))
    def fun2[TA, VA](f: (Result[TA, VA], Result[TA, VA]) => Result[TA, VA]) =
      new SDKNativeFunction(Fun2[TA, VA](f))
    def fun3[TA, VA](f: (Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA]) =
      new SDKNativeFunction(Fun3[TA, VA](f))
    def fun4[TA, VA](f: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA]) =
      new SDKNativeFunction(Fun4[TA, VA](f))
    def fun5[TA, VA](f: (
        Result[TA, VA],
        Result[TA, VA],
        Result[TA, VA],
        Result[TA, VA],
        Result[TA, VA]
    ) => Result[TA, VA]) =
      new SDKNativeFunction(Fun5[TA, VA](f))
  }

  case class SDKNativeInnerFunction[TA, VA](function: NativeFunctionSignatureAdv[TA, VA])
      extends SDKValue[TA, VA] {
    def arguments = function.numArgs
  }

  case class SDKNativeValue[TA, VA](value: Result[TA, VA]) extends SDKValue[TA, VA]
}

sealed trait StoredValue[TA, VA]
object StoredValue {
  case class Eager[TA, VA](value: Result[TA, VA]) extends StoredValue[TA, VA]
  case class Lazy[TA, VA](
      toEvaluate: Definition[TA, VA],
      parentContext: CallStackFrame[TA, VA],
      siblings: Map[Name, Definition[TA, VA]]
  ) extends StoredValue[TA, VA]
}

final case class CallStackFrame[TA, VA](
    bindings: Map[Name, StoredValue[TA, VA]],
    parent: Option[CallStackFrame[TA, VA]]
) {
  def get(name: Name): Option[StoredValue[TA, VA]] =
    (bindings.get(name), parent) match {
      case (Some(res), _)            => Some(res)
      case (None, Some(parentFrame)) => parentFrame.get(name)
      case (None, None)              => None
    }
  def push(bindings: Map[Name, StoredValue[TA, VA]]): CallStackFrame[TA, VA] =
    CallStackFrame[TA, VA](bindings, Some(this))
}

final case class Store[TA, VA](
    definitions: Map[FQName, SDKValue[TA, VA]],
    ctors: Map[FQName, SDKConstructor[TA, VA]],
    callStack: CallStackFrame[TA, VA]
) {
  def getVariable(name: Name): Option[StoredValue[TA, VA]]  = callStack.get(name)
  def getDefinition(name: FQName): Option[SDKValue[TA, VA]] = definitions.get(name)
  def getCtor(name: FQName): Option[SDKConstructor[TA, VA]] = ctors.get(name)

  def push(bindings: Map[Name, StoredValue[TA, VA]]) = Store(definitions, ctors, callStack.push(bindings))
  def withBindingsFrom(other: Store[TA, VA]) = Store(definitions ++ other.definitions, ctors ++ other.ctors, callStack)
  def withDefinition(fqn: FQName, definition: SDKValue[TA, VA]): Store[TA, VA] =
    Store(definitions + (fqn -> definition), ctors, callStack)
  def withConstructor(fqn: FQName, constructor: SDKConstructor[TA, VA]): Store[TA, VA] =
    Store(definitions, ctors + (fqn -> constructor), callStack)
}

object Store {
  def fromDistributions(dists: Distribution*): Store[Unit, Type.UType] =
    dists.foldLeft(native) {
      case (acc, lib: Library) =>
        val packageName = lib.packageName
        lib.packageDef.modules.foldLeft(acc) { case (acc, (moduleName, module)) =>
          val withDefinitions = module.value.values.foldLeft(acc) { case (acc, (valueName, value)) =>
            val name       = FQName(packageName, moduleName, valueName)
            val definition = value.value.value
            val sdkDef     = SDKValue.SDKValueDefinition[Unit, Type.UType](definition)
            acc.withDefinition(name, sdkDef)
          }
          module.value.types.foldLeft(withDefinitions) { case (acc, (_, tpe)) =>
            val typeDef = tpe.value.value
            typeDef match {
              case Type.Definition.CustomType(_, accessControlledCtors) =>
                val ctors = accessControlledCtors.value.toMap
                ctors.foldLeft(acc) { case (acc, (ctorName, ctorArgs)) =>
                  val name = FQName(packageName, moduleName, ctorName)
                  acc.withConstructor(name, SDKConstructor[Unit, Type.UType](ctorArgs.map(_._2).toList))
                }
              case Type.Definition.TypeAlias(_, _) => acc
            }
          }
        }
    }

  def empty[TA, VA]: Store[TA, VA] =
    Store(Map(), Map(), CallStackFrame(Map(), None))
  def native: Store[Unit, UType] =
    Store(Native.native, Native.nativeCtors, CallStackFrame(Map(), None))
}

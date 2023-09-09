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
import org.finos.morphir.runtime.TypedMorphirRuntime.{RuntimeDefinition, TypeAttribs, ValueAttribs}
import zio.Chunk

sealed trait SDKValue

case class SDKConstructor(arguments: List[ValueAttribs])
object SDKValue {
  case class SDKValueDefinition(definition: RuntimeDefinition) extends SDKValue
  case class SDKNativeFunction(function: NativeFunctionSignature) extends SDKValue {
    def arguments = function.numArgs
  }

  object SDKNativeFunction {
    import NativeFunctionSignature._
    def fun1(f: Result => Result) =
      new SDKNativeFunction(Fun1(f))
    def fun2(f: (Result, Result) => Result) =
      new SDKNativeFunction(Fun2(f))
    def fun3(f: (Result, Result, Result) => Result) =
      new SDKNativeFunction(Fun3(f))
    def fun4(f: (Result, Result, Result, Result) => Result) =
      new SDKNativeFunction(Fun4(f))
    def fun5(f: (
        Result,
        Result,
        Result,
        Result,
        Result
    ) => Result) =
      new SDKNativeFunction(Fun5(f))
  }

  case class SDKNativeInnerFunction(function: NativeFunctionSignatureAdv)
      extends SDKValue {
    def arguments = function.numArgs
  }

  case class SDKNativeValue(value: Result) extends SDKValue
}

sealed trait StoredValue
object StoredValue {
  case class Eager(value: Result) extends StoredValue
  case class Lazy(
      toEvaluate: RuntimeDefinition,
      parentContext: CallStackFrame,
      siblings: Map[Name, RuntimeDefinition]
  ) extends StoredValue
}

final case class CallStackFrame(
    bindings: Map[Name, StoredValue],
    parent: Option[CallStackFrame]
) {
  def get(name: Name): Option[StoredValue] =
    (bindings.get(name), parent) match {
      case (Some(res), _)            => Some(res)
      case (None, Some(parentFrame)) => parentFrame.get(name)
      case (None, None)              => None
    }
  def push(bindings: Map[Name, StoredValue]): CallStackFrame =
    CallStackFrame(bindings, Some(this))
}

final case class Store(
    callStack: CallStackFrame
) {
  def getVariable(name: Name): Option[StoredValue] = callStack.get(name)
  def push(bindings: Map[Name, StoredValue])       = Store(callStack.push(bindings))
}

object Store {
  def empty = Store(CallStackFrame(Map(), None))
}

final case class GlobalDefs(
    definitions: Map[FQName, SDKValue],
    ctors: Map[FQName, SDKConstructor]
) {
  def getDefinition(name: FQName): Option[SDKValue] = definitions.get(name)
  def withBindingsFrom(other: GlobalDefs) =
    GlobalDefs(definitions ++ other.definitions, ctors ++ other.ctors)
  def withDefinition(fqn: FQName, definition: SDKValue): GlobalDefs =
    GlobalDefs(definitions + (fqn -> definition), ctors)
  def withConstructor(fqn: FQName, constructor: SDKConstructor): GlobalDefs =
    GlobalDefs(definitions, ctors + (fqn -> constructor))
  def getCtor(name: FQName): Option[SDKConstructor] = ctors.get(name)
}

object GlobalDefs {
  def fromDistributions(dists: Distribution*): GlobalDefs =
    dists.foldLeft(native) {
      case (acc, lib: Library) =>
        val packageName = lib.packageName
        lib.packageDef.modules.foldLeft(acc) { case (acc, (moduleName, module)) =>
          val withDefinitions = module.value.values.foldLeft(acc) { case (acc, (valueName, value)) =>
            val name       = FQName(packageName, moduleName, valueName)
            val definition = value.value.value
            val sdkDef     = SDKValue.SDKValueDefinition(definition)
            acc.withDefinition(name, sdkDef)
          }
          module.value.types.foldLeft(withDefinitions) { case (acc, (_, tpe)) =>
            val typeDef = tpe.value.value
            typeDef match {
              case Type.Definition.CustomType(_, accessControlledCtors) =>
                val ctors = accessControlledCtors.value.toMap
                ctors.foldLeft(acc) { case (acc, (ctorName, ctorArgs)) =>
                  val name = FQName(packageName, moduleName, ctorName)
                  acc.withConstructor(name, SDKConstructor(ctorArgs.map(_._2).toList))
                }
              case Type.Definition.TypeAlias(_, _) => acc
            }
          }
        }
    }

  def empty: GlobalDefs =
    GlobalDefs(Map(), Map())
  def native: GlobalDefs =
    GlobalDefs(Native.native, Native.nativeCtors)
}

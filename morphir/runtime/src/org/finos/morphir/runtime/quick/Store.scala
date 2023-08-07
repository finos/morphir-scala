package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.naming.FQName.getLocalName
import org.finos.morphir.naming.Name.toTitleCase
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.{Module, Type}
import zio.Chunk

sealed trait SDKValue[TA, VA]

case class SDKConstructor[TA, VA](arguments: List[VA])
object SDKValue {
  case class SDKValueDefinition[TA, VA](definition: Definition[TA, VA]) extends SDKValue[TA, VA]
  case class SDKNativeFunction[TA, VA](arguments: Int, function: Any)   extends SDKValue[TA, VA]
  case class SDKNativeValue[TA, VA](value: Result[TA, VA])              extends SDKValue[TA, VA]
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
}

object Store {
  def fromLibrary(lib: Library): Store[Unit, Type.UType] = {
    val packageName = lib.packageName
    val valueBindings = lib.packageDef.modules.flatMap { case (moduleName, accessControlledModule) =>
      accessControlledModule.value.values.map {
        case (localName, accessControlledValue) =>
          val name       = FQName(packageName, moduleName, localName)
          val definition = accessControlledValue.value.value
          val sdkDef     = SDKValue.SDKValueDefinition(definition)
          (name, sdkDef)
      }
    }

    val ctorBindings: Map[FQName, SDKConstructor[Unit, Type.UType]] =
      lib.packageDef.modules.flatMap { case (moduleName, accessControlledModule) =>
        accessControlledModule.value.types.flatMap {
          case (localName, accessControlledType) =>
            val definition = accessControlledType.value.value
            definition match {
              case Type.Definition.CustomType(_, accessControlledCtors) =>
                val ctors = accessControlledCtors.value.toMap
                ctors.map { case (ctorName, ctorArgs) =>
                  val name = FQName(packageName, moduleName, ctorName)
                  (name, SDKConstructor[Unit, Type.UType](ctorArgs.map(_._2).toList))
                }
              case Type.Definition.TypeAlias(_, _) => Map.empty
            }
        }
      }
    Store(valueBindings ++ Native.native, ctorBindings ++ Native.nativeCtors, CallStackFrame(Map(), None))
  }

  def empty[TA, VA]: Store[TA, VA] = {
    val plus: SDKValue[TA, VA] = SDKValue.SDKNativeFunction(
      2,
      (a: Result[TA, VA], b: Result[TA, VA]) =>
        Result.Primitive(Result.unwrap(a).asInstanceOf[Long] + Result.unwrap(b).asInstanceOf[Long])
    )

    val lessThan: SDKValue[TA, VA] = SDKValue.SDKNativeFunction(
      2,
      (a: Result[TA, VA], b: Result[TA, VA]) =>
        Result.Primitive(Result.unwrap(a).asInstanceOf[Long] < Result.unwrap(b).asInstanceOf[Long])
    )
    val native = Map(
      FQName.fromString("Morphir.SDK:Basics:add")      -> plus,
      FQName.fromString("Morphir.SDK:Basics:lessThan") -> lessThan
    )
    Store(native, Map(), CallStackFrame(Map(), None))
  }
}

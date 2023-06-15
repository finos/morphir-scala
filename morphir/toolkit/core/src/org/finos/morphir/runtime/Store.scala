package org.finos.morphir
package runtime

import org.finos.morphir.ir.internal.*
import org.finos.morphir.ir.internal.Value.{List as ListValue, *}
import org.finos.morphir.ir.{FQName, Name, Type}
import ir.Value.{TypedValue, Value}
import org.finos.morphir.ir.FQName.getLocalName
import org.finos.morphir.ir.Name.toTitleCase
import org.finos.morphir.ir.Value.Pattern

sealed trait ResultValue[TA, VA]



object ResultValue {
  def unwrap[TA, VA](arg: ResultValue[TA, VA]): Any =
    arg match {
      case Unit()               => ()
      case Primitive(value)     => value
      case ListResult(elements) => elements.map(unwrap(_))
      case TupleResult(elements) =>
        val listed = Helpers.tupleToList(elements).getOrElse(throw new Exception("Invalid tuple returned to top level"))
        val mapped = listed.map(unwrap(_))
        Helpers.listToTuple(mapped)
      case RecordResult(elements) => elements.map { case (name, value) => name.toCamelCase -> unwrap(value) }
      case ConstructorResult(name, values) => (toTitleCase(name.localName), values.map(unwrap(_))) //Just for testing
      case other =>
        throw new Exception(s"$other returned to top level, only Unit, Primitive, List, Tuples, Constructed Types and Records are supported")
    }
  case class Unit[TA, VA]()                extends ResultValue[TA, VA]
  case class Primitive[TA, VA](value: Any) extends ResultValue[TA, VA]

  case class TupleResult[TA, VA](elements: Any) extends ResultValue[TA, VA]

  case class RecordResult[TA, VA](elements: Map[Name, ResultValue[TA, VA]]) extends ResultValue[TA, VA]
  // Should not be Map[String, Any] - pass on that for a while

  case class ListResult[TA, VA](elements: List[ResultValue[TA, VA]]) extends ResultValue[TA, VA]

  case class Applied[TA, VA](body : Value[TA, VA],
                             curried: List[(Name, ResultValue[TA, VA])],
                             closingContext: CallStackFrame[TA, VA]
                            )

  case class FieldFunction[TA, VA](fieldName: Name) extends ResultValue[TA, VA]

  case class LambdaFunction[TA, VA](body: Value[TA, VA], pattern: Pattern[VA], closingContext: CallStackFrame[TA, VA])
      extends ResultValue[TA, VA]

  case class DefinitionFunction[TA, VA](
      body: Value[TA, VA],
      arguments: List[(Name, VA, Type.Type[TA])],
      curried: List[(Name, ResultValue[TA, VA])],
      closingContext: CallStackFrame[TA, VA]
  ) extends ResultValue[TA, VA] // TODO: needs param names, body

  case class ConstructorFunction[TA, VA](name: FQName, arguments: List[VA], curried: List[ResultValue[TA, VA]])
      extends ResultValue[TA, VA]
  case class ConstructorResult[TA, VA](name: FQName, values: List[ResultValue[TA, VA]]) extends ResultValue[TA, VA]

  // TODO: Figure out arguments list
  // case class NativeFunction[TA, VA](arguments : List[(Name, VA, Type.Type[TA])], curried : List[ResultValue[TA, VA]], function : Any) extends ResultValue[TA, VA]{
  case class NativeFunction[TA, VA](arguments: Int, curried: List[ResultValue[TA, VA]], function: Any)
      extends ResultValue[TA, VA] {}

}

sealed trait SDKValue[TA, VA]
object SDKValue {
  case class SDKValueDefinition[TA, VA](definition: Definition[TA, VA]) extends SDKValue[TA, VA]
  case class SDKConstructor[TA, VA](arguments: List[VA])                extends SDKValue[TA, VA]
  // case class SDKNativeFunction[TA, VA](arguments : List[(Name, VA, Type.Type[TA])], function : Any) extends SDKValue[TA, VA] TODO: Figure out arguments

  case class SDKNativeFunction[TA, VA](arguments: Int, function: Any) extends SDKValue[TA, VA]

  case class SDKNativeValue[TA, VA](value: ResultValue[TA, VA]) extends SDKValue[TA, VA]
}

sealed trait StoredValue[TA, VA]
object StoredValue {
  case class Eager[TA, VA](value: ResultValue[TA, VA]) extends StoredValue[TA, VA]

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

final case class Store[TA, VA](fqNameBindings: Map[FQName, SDKValue[TA, VA]], callStack: CallStackFrame[TA, VA]) {
  def get(name: Name): Option[StoredValue[TA, VA]]   = callStack.get(name)
  def get(name: FQName): Option[SDKValue[TA, VA]]    = fqNameBindings.get(name)
  def push(bindings: Map[Name, StoredValue[TA, VA]]) = Store(fqNameBindings, callStack.push(bindings))
}
object Store {
  def empty[TA, VA]: Store[TA, VA] = {
    val plus: SDKValue[TA, VA] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[TA, VA], b: ResultValue[TA, VA]) =>
        ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] + ResultValue.unwrap(b).asInstanceOf[Long])
    )

    val lessThan: SDKValue[TA, VA] = SDKValue.SDKNativeFunction(
      2,
      (a: ResultValue[TA, VA], b: ResultValue[TA, VA]) =>
        ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] < ResultValue.unwrap(b).asInstanceOf[Long])
    )
    val native = Map(
      FQName.fromString("Morphir.SDK:Basics:add") -> plus,
      FQName.fromString("Morphir.SDK:Basics:lessThan") -> lessThan
    )
    Store(native, CallStackFrame(Map(), None))
  }
}


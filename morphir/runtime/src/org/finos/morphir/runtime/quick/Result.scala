package org.finos.morphir.runtime.quick

import org.finos.morphir.naming._
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}
import Name.toTitleCase

import scala.collection.mutable

sealed trait Result[TA, VA] {
  def succinct(depth: Int): String = s"${this.getClass} (Default implementation)"
  def succinct: String             = succinct(2)
}

object Result {

  def unwrap[TA, VA](arg: Result[TA, VA]): Any =
    arg match {
      case Unit()               => ()
      case Primitive(value)     => value
      case ListResult(elements) => elements.map(unwrap(_))
      case SetResult(elements)  => elements.map(unwrap(_))
      case Tuple(elements) =>
        val listed = Helpers.tupleToList(elements).getOrElse(throw new Exception("Invalid tuple returned to top level"))
        val mapped = listed.map(unwrap(_))
        Helpers.listToTuple(mapped)
      case Record(elements)                => elements.map { case (name, value) => name.toCamelCase -> unwrap(value) }
      case MapResult(elements)             => elements.map { case (key, value) => unwrap(key) -> unwrap(value) }
      case ConstructorResult(name, values) => (toTitleCase(name.localName), values.map(unwrap(_)))
      case other =>
        throw new Exception(
          s"$other returned to top level, only Unit, Primitive, List, Maps, Sets, Tuples, Constructed Types and Records are supported"
        )
    }

  case class Unit[TA, VA]() extends Result[TA, VA] {
    override def succinct(depth: Int) = "Unit"
  }

  case class Primitive[TA, VA](value: Any) extends Result[TA, VA] {
    override def succinct(depth: Int) = s"Primitive($value)"
  }

  case class LocalDate[TA, VA](value: java.time.LocalDate) extends Result[TA, VA] {
    override def succinct(depth: Int) = s"LocalDate($value)"
  }

  case class LocalTime[TA, VA](value: java.time.LocalTime) extends Result[TA, VA] {
    override def succinct(depth: Int) = s"LocalTime($value)"
  }

  case class Tuple[TA, VA](elements: Any) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Tuple(...)"
    else {
      s"Tuple(${Helpers.tupleToList(elements).map((res: Any) => res.asInstanceOf[Result[TA, VA]]).map(_.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class SetResult[TA, VA](elements: mutable.LinkedHashSet[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Set(..)"
    else {
      s"Set(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class Record[TA, VA](elements: Map[Name, Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Record(..)"
    else {
      s"Record(${elements.map { case (key, value) => s"$key -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }

  case class ListResult[TA, VA](elements: List[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "List(..)"
    else {
      s"List(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class MapResult[TA, VA](elements: mutable.LinkedHashMap[Result[TA, VA], Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Dict(..)"
    else {
      s"Dict(${elements.map { case (key, value) => s"${key.succinct(depth - 1)} -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }
  case class Applied[TA, VA](
      body: Value[TA, VA],
      curried: List[(Name, Result[TA, VA])],
      closingContext: CallStackFrame[TA, VA]
  )

  case class FieldFunction[TA, VA](fieldName: Name) extends Result[TA, VA]

  case class LambdaFunction[TA, VA](body: Value[TA, VA], pattern: Pattern[VA], closingContext: CallStackFrame[TA, VA])
      extends Result[TA, VA]

  case class DefinitionFunction[TA, VA](
      body: Value[TA, VA],
      arguments: List[(Name, VA, Type.Type[TA])],
      curried: List[(Name, Result[TA, VA])],
      closingContext: CallStackFrame[TA, VA]
  ) extends Result[TA, VA]

  case class ConstructorFunction[TA, VA](name: FQName, arguments: List[VA], curried: List[Result[TA, VA]])
      extends Result[TA, VA]

  case class ConstructorResult[TA, VA](name: FQName, values: List[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) s"${name.toString}(..)"
    else {
      s"${name.toString}(${values.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class NativeFunction[TA, VA](arguments: Int, curried: List[Result[TA, VA]], function: Any)
      extends Result[TA, VA] {}
}

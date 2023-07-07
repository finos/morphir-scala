package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.MorphirIRFile

import SDKValue.{SDKNativeFunction, SDKNativeValue}

object EvaluatorQuick {

  def evaluate[TA, VA](ir: Value[TA, VA], store: Store[TA, VA]): Any = Result.unwrap(Loop.loop(ir, store))

  def evalFunction(entryFQName: FQName, store: Store[Unit, Type.UType], input: Any): Any = {
    val ir        = scalaToIR(input)
    val applyNode = V.apply(V.reference(entryFQName), ir) :> T.unit // lies but I don't think we check?
    evaluate[Unit, Type.UType](applyNode, store)
  }

  case class Record(values: Map[String, Any])
  object Record {
    def apply(args: (String, Any)*): Record = Record(args.toMap)
  }
  case class Constructor(name: String, arguments: List[Any])
  def curry(f: RawValue, args: List[RawValue]): RawValue =
    args match {
      case Nil          => f
      case head :: tail => curry(V.apply(f, head), tail)
    }
  def scalaToIR(value: Any): RawValue =
    value match {
      case ()                => V.unit
      case x: Int            => V.int(x)
      case s: String         => V.string(s)
      case b: Boolean        => V.boolean(b)
      case elements: List[_] => V.list(elements.map(scalaToIR(_)))
      case values: Map[_, _] =>
        val tuples = values.map { case (key, value) => V.tuple(scalaToIR(key), scalaToIR(value)) }.toSeq
        V.apply(V.reference(FQName.fromString("Morphir.SDK:Dict:fromList")), V.list(tuples: _*))
      case Record(values) =>
        val fields = values.map { case (name, value) => (name, scalaToIR(value)) }.toSeq
        V.recordRaw(fields: _*)
      case Constructor(name, args) =>
        val mappedArgs  = args.map(scalaToIR(_))
        val constructor = V.constructor(FQName.fromString(name))
        curry(constructor, mappedArgs)
      case other => throw new Exception(s"I don't know how to decompose $other")
    }

}

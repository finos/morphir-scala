package org.finos.morphir.runtime.quick

import org.finos.morphir.naming._
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.{Module, MorphirIRFile, Type}
import org.finos.morphir.runtime.*
import zio.Chunk

import scala.io.Source

object Dict {
  val fromList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (l: Result[Unit, Type.UType]) => {
      val list = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      val mapped = list
        .map { input =>
          val asTuple = input
            .asInstanceOf[Result.Tuple[Unit, Type.UType]]
            .elements
            .asInstanceOf[(Result[Unit, Type.UType], Result[Unit, Type.UType])]
          asTuple._1 -> asTuple._2
        }
        .toMap
      Result.MapResult(mapped)
    }
  )

  val get: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (key: Result[Unit, Type.UType], m: Result[Unit, Type.UType]) => {
      val map = m.asInstanceOf[Result.MapResult[Unit, Type.UType]].elements
      map.get(key) match {
        case Some(value) => Result.ConstructorResult(
            FQName.fromString("Morphir.SDK:Maybe:just"),
            List(value)
          )
        case None => Result.ConstructorResult(
            FQName.fromString("Morphir.SDK:Maybe:nothing"),
            List()
          )
      }
    }
  )

  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Dict:fromList") -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")      -> get
  )
}

object String {
  val append: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[String] + Result.unwrap(b).asInstanceOf[String])
  )
  val right: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(b).asInstanceOf[String].takeRight(Result.unwrap(a).asInstanceOf[Long].toInt))
  )
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:String:append") -> append,
    FQName.fromString("Morphir.SDK:String:right")  -> right
  )
}
object Native {
  val and: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[Boolean] && Result.unwrap(b).asInstanceOf[Boolean])
  )
  val or: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[Boolean] || Result.unwrap(b).asInstanceOf[Boolean])
  )
  val plus: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[Long] + Result.unwrap(b).asInstanceOf[Long])
  )
  val subtract: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[Long] - Result.unwrap(b).asInstanceOf[Long])
  )
  val divide: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.unwrap(a) match {
        case l: Long =>
          Result.Primitive(l / Result.unwrap(b).asInstanceOf[Long])
        case l: Double =>
          Result.Primitive(l / Result.unwrap(b).asInstanceOf[Double])
      }
  )
  val negate: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: Result[Unit, Type.UType]) => Result.Primitive(-Result.unwrap(a).asInstanceOf[Long])
  )

  val toFloat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: Result[Unit, Type.UType]) => Result.Primitive(Result.unwrap(a).asInstanceOf[Long].toDouble)
  )
  val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) => {
      val denominator = Math.log(Result.unwrap(a).asInstanceOf[Double])
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(Result.unwrap(b).asInstanceOf[Double]) / denominator
      Result.Primitive(asDouble)
    }
  )

  val lessThan: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).asInstanceOf[Long] < Result.unwrap(b).asInstanceOf[Long])
  )

  val equal: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive(a == b)
  )
  val cons: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) => {
      val listB = b.asInstanceOf[Result.ListResult[Unit, Type.UType]]
      Result.ListResult(a :: listB.elements)
    }
  )
  val concat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (l: Result[Unit, Type.UType]) => {
      val list      = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      val flattened = list.flatMap(inner => inner.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements)
      Result.ListResult(flattened)
    }
  )
  val map: SDKValue[Unit, Type.UType] = SDKValue.SDKValueDefinition(
    MapImpl.ir
  )
//  val map: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
//    2,
//    (f: ResultValue[Unit, Type.UType], l: ResultValue[Unit, Type.UType]) => {
//      val list = l.asInstanceOf[ResultValue.ListResult[Unit, Type.UType]]
//
//    }
//  )
  val fromParts: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    3,
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType], c: Result[Unit, Type.UType]) =>
      Result.LocalDate(java.time.LocalDate.of(Result.unwrap(a).asInstanceOf[Long].toInt, Result.unwrap(b).asInstanceOf[Long].toInt, Result.unwrap(c).asInstanceOf[Long].toInt))
  )
  val fromMilliseconds: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: Result[Unit, Type.UType]) =>
      Result.LocalTime(java.time.LocalTime.of(0, 0).plusNanos(Result.unwrap(a),asInstanceOf[Long] * 1000))
  )

  val pi: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(Result.Primitive(3.toDouble))

  val just: SDKConstructor[Unit, Type.UType]    = SDKConstructor(List(Type.variable("contents")))
  val nothing: SDKConstructor[Unit, Type.UType] = SDKConstructor(List())
  val ok: SDKConstructor[Unit, Type.UType]      = SDKConstructor(List(Type.variable("contents")))
  val err: SDKConstructor[Unit, Type.UType]     = SDKConstructor(List(Type.variable("contents")))

  val nativeCtors: Map[FQName, SDKConstructor[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Maybe:just")    -> just,
    FQName.fromString("Morphir.SDK:Maybe:nothing") -> nothing,
    FQName.fromString("Morphir.SDK:Result:ok")     -> ok,
    FQName.fromString("Morphir.SDK:Result:err")    -> err
  )

  val native: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Basics:equal")    -> equal,
    FQName.fromString("Morphir.SDK:Basics:and")      -> and,
    FQName.fromString("Morphir.SDK:Basics:or")       -> or,
    FQName.fromString("Morphir.SDK:Basics:pi")       -> pi,
    FQName.fromString("Morphir.SDK:Basics:add")      -> plus,
    FQName.fromString("Morphir.SDK:Basics:subtract") -> subtract,
    FQName.fromString("Morphir.SDK:Basics:divide")   -> divide,
    FQName.fromString("Morphir.SDK:Basics:negate")   -> negate,
    FQName.fromString("Morphir.SDK:Basics:toFloat")  -> toFloat,
    FQName.fromString("Morphir.SDK:Basics:logBase")  -> log,
    FQName.fromString("Morphir.SDK:Basics:lessThan") -> lessThan,
    FQName.fromString("Morphir.SDK:List:cons")       -> cons,
    FQName.fromString("Morphir.SDK:List:concat")     -> concat,
    FQName.fromString("Morphir.SDK:List:map")        -> map,
      FQName.fromString("Morphir.SDK:LocalDate:fromParts") -> fromParts
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  ) ++ Dict.sdk ++ String.sdk
}

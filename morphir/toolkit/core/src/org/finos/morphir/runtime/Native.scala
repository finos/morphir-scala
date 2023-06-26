package org.finos.morphir.runtime

import org.finos.morphir.ir.{FQName, Module, MorphirIRFile, Name, QName, Type}
import org.finos.morphir.ir.distribution.Distribution.Library
import zio.Chunk

import scala.io.Source



object Native {
  val plus: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
      ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] + ResultValue.unwrap(b).asInstanceOf[Long])
  )
  val subtract: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
      ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] - ResultValue.unwrap(b).asInstanceOf[Long])
  )
  val negate: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: ResultValue[Unit, Type.UType]) => ResultValue.Primitive(-ResultValue.unwrap(a).asInstanceOf[Long])
  )
  val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) => {
      val denominator = Math.log(ResultValue.unwrap(a).asInstanceOf[Double])
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(ResultValue.unwrap(b).asInstanceOf[Double]) / denominator
      ResultValue.Primitive(asDouble)
    }
  )

  val lessThan: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) =>
      ResultValue.Primitive(ResultValue.unwrap(a).asInstanceOf[Long] < ResultValue.unwrap(b).asInstanceOf[Long])
  )
  val cons: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    2,
    (a: ResultValue[Unit, Type.UType], b: ResultValue[Unit, Type.UType]) => {
      val listB = b.asInstanceOf[ResultValue.ListResult[Unit, Type.UType]]
      ResultValue.ListResult(a :: listB.elements)
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


  val pi: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(ResultValue.Primitive(3L))

  val native: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Basics:pi") -> pi,
    FQName.fromString("Morphir.SDK:Basics:add") -> plus,
    FQName.fromString("Morphir.SDK:Basics:subtract") -> subtract,
    FQName.fromString("Morphir.SDK:Basics:negate") -> negate,
    FQName.fromString("Morphir.SDK:Basics:logBase") -> log,
    FQName.fromString("Morphir.SDK:Basics:lessThan") -> lessThan,
    FQName.fromString("Morphir.SDK:List:cons") -> cons,
    FQName.fromString("Morphir.SDK:List:map") -> map,
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  )
}

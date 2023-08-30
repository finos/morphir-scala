package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Type
import org.finos.morphir.naming._
import org.finos.morphir.runtime.UnsupportedType

import scala.collection.mutable

object DictSDK {
  val fromList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      val mapped = list
        .map { input =>
          val asTuple = input
            .asInstanceOf[Result.Tuple[Unit, Type.UType]]
            .elements
            .asInstanceOf[(Result[Unit, Type.UType], Result[Unit, Type.UType])]
          asTuple._1 -> asTuple._2
        }
        .to(mutable.LinkedHashMap)
      Result.MapResult(mapped)
  }

  val get: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (key: Result[Unit, Type.UType], m: Result[Unit, Type.UType]) =>
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

  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Dict:fromList") -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")      -> get
  )
}
object ListSDK {
  val append: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      val listA = a.asInstanceOf[Result.ListResult[Unit, Type.UType]]
      val listB = b.asInstanceOf[Result.ListResult[Unit, Type.UType]]
      Result.ListResult(listA.elements.appendedAll(listB.elements))
  }

  val cons: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      val listB = b.asInstanceOf[Result.ListResult[Unit, Type.UType]]
      Result.ListResult(a :: listB.elements)
  }
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:List:append") -> append,
    FQName.fromString("Morphir.SDK:List:cons")   -> cons
  )
}
object BasicsSDK {
  val append: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2(
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      (a, b) match {
        case (Result.ListResult(aElements), Result.ListResult(bElements)) =>
          Result.ListResult(aElements.appendedAll(bElements))
        case (Result.Primitive.String(a), Result.Primitive.String(b)) => Result.Primitive.String(a + b)
        case (other1, other2) => throw new UnsupportedType(s"Append called on unrecognized types: $other1, $other2")
      }
  )
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Basics:append") -> append
  )

}
object SetSDK {
  val fromList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      Result.SetResult(list.to(mutable.LinkedHashSet))
  }

  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Set:fromList") -> fromList
  )
}

object StringSDK {
  val append: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2((a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.String(a.unwrapString + b.unwrapString)
    )
  val left: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2((a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) = {
      val stringLength = Result.unwrap(b).asInstanceOf[String].length
      Result.Primitive(
        Result.unwrap(b).asInstanceOf[String].dropRight(stringLength - Result.unwrap(a).asInstanceOf[Long].toInt)
      )
    }
  )
  val right: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2((a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.String(b.unwrapString.takeRight(a.unwrapInt))
    )
  val fromInt: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: Result[Unit, Type.UType]) =>
      Result.Primitive(Result.unwrap(a).toString)
  )
  val fromFloat: SDKValue[Unit, Type.UType] = fromInt
  val toInt: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction(
    1,
    (a: Result[Unit, Type.UType]) => {
      val optional = Result.unwrap(a).asInstanceOf[String].toIntOption
      optional match {
        case Some(value) => Result.ConstructorResult(
            FQName.fromString("Morphir.SDK:Maybe:just"),
            List(Result.Primitive[Unit, Type.UType](value))
          )
        case None => Result.ConstructorResult(
            FQName.fromString("Morphir.SDK:Maybe:nothing"),
            List()
          )
      }
    }
  )
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:String:append")    -> append,
    FQName.fromString("Morphir.SDK:String:left")      -> left,
    FQName.fromString("Morphir.SDK:String:right")     -> right,
    FQName.fromString("Morphir.SDK:String:fromInt")   -> fromInt,
    FQName.fromString("Morphir.SDK:String:fromFloat") -> fromFloat,
    FQName.fromString("Morphir.SDK:String:toInt")     -> toInt
  )
}
object Native {
  private def handleSameNumerics(
      a: Result[Unit, Type.UType],
      b: Result[Unit, Type.UType]
  )(f: (Any, Any, scala.Numeric[Any]) => Any): Result.Primitive[Unit, Type.UType, _] = {
    val components = Result.unwrapNumericsWithHelper[Unit, Type.UType, Any](a, b)
    Result.Primitive.makeOrFail[Unit, Type.UType, Any](f(components.a, components.b, components.helper))
  }

  private def handleBooleans(
      a: Result[Unit, Type.UType],
      b: Result[Unit, Type.UType]
  )(f: (Boolean, Boolean) => Boolean) = {
    val aBool = Result.unwrapBoolean(a)
    val bBool = Result.unwrapBoolean(b)
    Result.Primitive.Boolean[Unit, Type.UType](f(aBool, bBool))
  }

  val and: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleBooleans(a, b)(_ && _)
    }
  val or: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleBooleans(a, b)(_ || _)
    }
  val not: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1((a: Result[Unit, Type.UType]) =>
    Result.Primitive.Boolean(!a.unwrapBoolean)
  )
  val plus: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.plus(aNum, bNum) }
    }
  val subtract: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.minus(aNum, bNum) }
    }
  val divide: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) =>
        // scala.Numeric does not handle division, it's part of scala.Fractional or scala.Integral, need to get the right one
        val components        = Result.unwrapNumericsWithHelper[Unit, Type.UType, Any](a, b)
        val divideIntegers    = components.integralHelper.map(helper => helper.quot(components.a, components.b))
        val divideFractionals = components.fractionalHelper.map(helper => helper.div(components.a, components.b))
        divideIntegers
          .orElse(divideFractionals)
          .getOrElse {
            throw new Exception(
              s"Cannot find a scala.Fractional or scala.Integral for the value inside of a `${a}` (and `${b}`) instance type. This should not be possible"
            )
          }
      }
    }
  val multiply: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.times(aNum, bNum) }
    }
  val negate: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (a: Result[Unit, Type.UType]) =>
      val components = Result.unwrapNumericWithHelper[Unit, Type.UType, Any](a)
      Result.Primitive.makeOrFail(components.helper.negate(components.value))
  }

  val toFloat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (a: Result[Unit, Type.UType]) => Result.Primitive.Double(Result.unwrap(a).asInstanceOf[Long].toDouble)
  }
  val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      val denominator = Math.log(Result.unwrap(a).asInstanceOf[Double])
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(Result.unwrap(b).asInstanceOf[Double]) / denominator
      Result.Primitive.Double(asDouble)
  }

  val lessThan: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.lt(a, b) }
    }

  val greaterThan: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.gt(a, b) }
    }

  val equal: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.Boolean(a == b)
  }
  val concat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list      = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      val flattened = list.flatMap(inner => inner.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements)
      Result.ListResult(flattened)
  }
  val singleton: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      Result.ListResult(List(l))
  }
  val isEmpty: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list = l.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      Result.Primitive.Boolean(list.length == 0)
  }
  val filter: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (store: Store[Unit, Type.UType]) => (f: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
        val list = l.unwrapList
        val out =
          list.filter(elem =>
            Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), f, elem, store).unwrapBoolean
          )
        Result.ListResult(out)
    }
  }

  val map: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (store: Store[Unit, Type.UType]) => (f: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
        val list = l.unwrapList
        val out =
          list.map(elem =>
            Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), f, elem, store)
          )
        Result.ListResult(out)
    }
  }

  val fromParts: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun3 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType], c: Result[Unit, Type.UType]) =>
      Result.LocalDate(java.time.LocalDate.of(a.unwrapInt, b.unwrapInt, c.unwrapInt))
  }

  val utc = java.time.ZoneId.of("UTC")

  def fromMillisecondsEpoch(millis: Long): java.time.LocalTime =
    java.time.Instant.ofEpochMilli(millis).atZone(utc).toLocalTime()

  def fromMillisecondsNanos(millis: Long): java.time.LocalTime =
    java.time.LocalTime.of(0, 0).plusNanos(millis * 1000000)

  val fromMilliseconds: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (a: Result[Unit, Type.UType]) =>
      val millis = Result.unwrap(a).asInstanceOf[Long]
      val time   = fromMillisecondsEpoch(millis)
      Result.LocalTime(time)
  }

  val pi: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(Result.Primitive.Double(3.toDouble))

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
    FQName.fromString("Morphir.SDK:Basics:equal")               -> equal,
    FQName.fromString("Morphir.SDK:Basics:and")                 -> and,
    FQName.fromString("Morphir.SDK:Basics:or")                  -> or,
    FQName.fromString("Morphir.SDK:Basics:not")                 -> not,
    FQName.fromString("Morphir.SDK:Basics:pi")                  -> pi,
    FQName.fromString("Morphir.SDK:Basics:add")                 -> plus,
    FQName.fromString("Morphir.SDK:Basics:subtract")            -> subtract,
    FQName.fromString("Morphir.SDK:Basics:divide")              -> divide,
    FQName.fromString("Morphir.SDK:Basics:multiply")            -> multiply,
    FQName.fromString("Morphir.SDK:Basics:negate")              -> negate,
    FQName.fromString("Morphir.SDK:Basics:toFloat")             -> toFloat,
    FQName.fromString("Morphir.SDK:Basics:logBase")             -> log,
    FQName.fromString("Morphir.SDK:Basics:lessThan")            -> lessThan,
    FQName.fromString("Morphir.SDK:Basics:greaterThan")         -> greaterThan,
    FQName.fromString("Morphir.SDK:List:concat")                -> concat,
    FQName.fromString("Morphir.SDK:List:singleton")             -> singleton,
    FQName.fromString("Morphir.SDK:List:isEmpty")               -> isEmpty,
    FQName.fromString("Morphir.SDK:List:map")                   -> map,
    FQName.fromString("Morphir.SDK:List:filter")                -> filter,
    FQName.fromString("Morphir.SDK:LocalDate:fromParts")        -> fromParts,
    FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds") -> fromMilliseconds
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  ) ++ DictSDK.sdk ++ SetSDK.sdk ++ StringSDK.sdk ++ ListSDK.sdk ++ BasicsSDK.sdk
}

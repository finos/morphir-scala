package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Type
import org.finos.morphir.naming._
import org.finos.morphir.runtime.{IllegalValue, UnexpectedType, UnsupportedType}
import org.finos.morphir.runtime.quick.Result.Primitive
import org.finos.morphir.runtime.Extractors._
import scala.collection.mutable

object DictSDK {
  val filter: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (store: Store[Unit, Type.UType]) => (f: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
        {
          val dictMap = l.unwrapMap
          val newDict =
            dictMap.filter { case (k, v) =>
              val partialAppliedF =
                Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), f, k, store)
              val result =
                Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), partialAppliedF, v, store)
              result.unwrapBoolean
            }
          Result.MapResult(newDict)
        }
    }
  }

  val fromList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list = l.unwrapList
      val mappedList = list
        .map { input =>
          input.unwrapTuple match {
            case TupleSigniture.Tup2((a, b)) => (a, b)
            case _ =>
              throw new IllegalValue(s"Input to Dict.fromList was not a Tuple2-based element, it was: `$input`")
          }
        }
      Result.MapResult(mutable.LinkedHashMap(mappedList: _*))
  }

  val toList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (d: Result[Unit, Type.UType]) =>
      val dict     = d.unwrapMap
      val elements = dict.toList.map { case (k, v) => Result.Tuple(TupleSigniture.Tup2((k, v))) }
      Result.ListResult(elements)
  }

  val empty: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(Result.MapResult(mutable.LinkedHashMap()))

  val get: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (key: Result[Unit, Type.UType], m: Result[Unit, Type.UType]) =>
      val map = m.unwrapMap
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

  val singleton: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (key: Result[Unit, Type.UType], value: Result[Unit, Type.UType]) =>
      Result.MapResult(mutable.LinkedHashMap.from(List(key -> value)))
  }

  val keys: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (m: Result[Unit, Type.UType]) =>
      val map = m.unwrapMap
      Result.ListResult(map.keys.toList)
  }

  private def optionToMaybe(opt: Option[Result[Unit, Type.UType]]): Result[Unit, Type.UType] =
    opt match {
      case Some(value) => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:just"),
          List(value)
        )
      case None => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:nothing"),
          List()
        )
    }

  val update: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      // update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
      // update targetKey alter dictionary =
      // case alter(get targetKey dictionary) of
      //   Just value -> insert targetKey value dictionary
      //   Nothing    -> remove targetKey dictionary
      (store: Store[Unit, Type.UType]) => (
          targetKeyRaw: Result[Unit, Type.UType],
          alterRaw: Result[Unit, Type.UType],
          dictRaw: Result[Unit, Type.UType]
      ) =>
        val dict      = dictRaw.unwrapMap.clone() // make sure to clone it to not modify original one
        val currValue = optionToMaybe(dict.get(targetKeyRaw))
        val newValue  = Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), alterRaw, currValue, store)

        newValue match {
          case Result.ConstructorResult(FQString("Morphir.SDK:Maybe:just"), List(value)) =>
            dict += ((targetKeyRaw, value))
          case Result.ConstructorResult(FQString("Morphir.SDK:Maybe:nothing"), _) =>
            dict.remove(targetKeyRaw)
          case _ =>
            throw new IllegalValue(s"Expected a Result.Constructor of Morphir.SDK:Maybe:just/nothing but got $newValue")
        }
        Result.MapResult(dict)
    }
  }

  val insert: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun3 {
    (key: Result[Unit, Type.UType], value: Result[Unit, Type.UType], m: Result[Unit, Type.UType]) =>
      val map = m.unwrapMap.clone() // because LinkedHashMap is mutable MAKE SURE TO CLONE IT before inserting things
      map += ((key, value))
      Result.MapResult(map)
  }

  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Dict:empty")     -> empty,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:filter")    -> filter,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")       -> get,
    FQName.fromString("Morphir.SDK:Dict:insert")    -> insert,
    FQName.fromString("Morphir.SDK:Dict:keys")      -> keys,
    FQName.fromString("Morphir.SDK:Dict:toList")    -> toList,
    FQName.fromString("Morphir.SDK:Dict:singleton") -> singleton,
    FQName.fromString("Morphir.SDK:Dict:update")    -> update
  )
}

object ListSDK {
  val foldl: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      (store: Store[Unit, Type.UType]) =>
        (f: Result[Unit, Type.UType], first: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
          {
            val list = l.unwrapList
            list.foldLeft(first) { (b, a) => // Note that elm does (a, b) => b, scala does it in the opposite way
              val partialAppliedF =
                Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), f, a, store)
              val result =
                Loop.handleApplyResult[Unit, Type.UType](Type.UType.Unit(()), partialAppliedF, b, store)

              result
            }
          }
    }
  }
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
  val isEmpty: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
      val listA = arg.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      Result.Primitive.Boolean(listA.size == 0)
    }
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:List:foldl")   -> foldl,
    FQName.fromString("Morphir.SDK:List:append")  -> append,
    FQName.fromString("Morphir.SDK:List:cons")    -> cons,
    FQName.fromString("Morphir.SDK:List:isEmpty") -> isEmpty
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
  val length: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun1((arg: Result[Unit, Type.UType]) =>
      Result.Primitive.Int(arg.unwrapString.length)
    )
  val append: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2((a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.String(a.unwrapString + b.unwrapString)
    )
  val left: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.String(
        b.unwrapString.dropRight(b.unwrapString.length - a.unwrapInt)
      )
    }
  val right: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2((a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      Result.Primitive.String(b.unwrapString.takeRight(a.unwrapInt))
    )
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:String:append") -> append,
    FQName.fromString("Morphir.SDK:String:right")  -> right,
    FQName.fromString("Morphir.SDK:String:length") -> length
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
    (a: Result[Unit, Type.UType]) =>
      val output =
        a.unwrapNumeric match {
          case Primitive.Int(value)        => value.toDouble
          case Primitive.Long(value)       => value.toDouble
          case Primitive.Double(value)     => value
          case Primitive.BigDecimal(value) => value.toDouble
          case Primitive.Float(value)      => value.toDouble
        }
      Result.Primitive.Double(output)
  }
  val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      val denominator = Math.log(a.unwrapDouble)
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(b.unwrapDouble) / denominator
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
      val list = l.unwrapList
      Result.Primitive.Boolean(list.length == 0)
  }

  val length: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (l: Result[Unit, Type.UType]) =>
      val list = l.unwrapList
      Result.Primitive.Int(list.length)
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
    FQName.fromString("Morphir.SDK:List:length")                -> length,
    FQName.fromString("Morphir.SDK:LocalDate:fromParts")        -> fromParts,
    FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds") -> fromMilliseconds
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  ) ++ DictSDK.sdk ++ SetSDK.sdk ++ StringSDK.sdk ++ ListSDK.sdk ++ BasicsSDK.sdk
}

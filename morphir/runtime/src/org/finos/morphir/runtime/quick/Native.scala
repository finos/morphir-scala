package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.{RTValue, SDKConstructor, SDKValue}
import org.finos.morphir.runtime.MorphirRuntimeError.{IllegalValue, UnexpectedType, UnsupportedType, WrongArgumentTypes}
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.internal.{InvokeableEvaluator, NativeFunctionSignatureAdv}
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.sdk.DecimalSDK.{minusOne, one, zero}
import org.finos.morphir.runtime.sdk.AggregateSDK.count

import scala.collection.mutable

object DictSDK {
  // === ELM Function ===
  // filter : (k -> v -> Bool) -> Dict k v -> Dict comparable v
  // filter isGood dict =
  //   foldl (\k v d -> if isGood k v then insert k v d else d) empty dict
  val filter: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: InvokeableEvaluator) => (isGood: RTValue, dictRaw: RTValue) =>
        {
          val dictMap = dictRaw.coerceMap.value
          val newDict =
            dictMap.filter { case (k, v) =>
              val result = evaluator.handleApplyResult2(Type.UType.Unit(()), isGood, k, v)
              result.coerceBoolean.value
            }
          RTValue.Map(newDict)
        }
    }
  }

  val fromList: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: RTValue) =>
      val list = l.coerceList.value
      val mappedList = list
        .map { input =>
          // unwrap the element that is in the list provided to the Dict.fromList function. It has to be a Tuple2
          input.coerceTuple.value match {
            case List(a, b) => (a, b)
            case _ =>
              throw new UnexpectedType(
                s"Tuple2-based element",
                input,
                hint = "Expected because this was passed to Dict.fromList"
              )
          }
        }
      RTValue.Map(mutable.LinkedHashMap(mappedList: _*))
  }

  val toList: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (d: RTValue) =>
      val dict     = d.coerceMap.value
      val elements = dict.toList.map { case (k, v) => RTValue.Tuple(k, v) }
      RTValue.List(elements)
  }

  val empty: SDKValue = SDKValue.SDKNativeValue(RTValue.Map(mutable.LinkedHashMap()))

  val get: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (key: RTValue, m: RTValue) =>
      val map = m.coerceMap.value
      optionToMaybe(map.get(key))
  }
  val member: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (key: RTValue, m: RTValue) =>
      val map = m.coerceMap.value
      RTValue.Primitive.Boolean(map.contains(key))
  }

  val isEmpty: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (m: RTValue) =>
      val map = m.coerceMap.value
      RTValue.Primitive.Boolean(map.isEmpty)
  }

  val size: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (m: RTValue) =>
      val map = m.coerceMap.value
      RTValue.Primitive.Int(map.size)
  }

  val singleton: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (key: RTValue, value: RTValue) =>
      RTValue.Map(mutable.LinkedHashMap.from(List(key -> value)))
  }

  val keys: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (m: RTValue) =>
      val map = m.coerceMap.value
      RTValue.List(map.keys.toList)
  }

  val values: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (m: RTValue) =>
      val map = m.coerceMap.value
      RTValue.List(map.values.toList)
  }

  private def optionToMaybe(opt: Option[RTValue]): RTValue =
    opt match {
      case Some(value) => RTValue.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:just"),
          List(value)
        )
      case None => RTValue.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:nothing"),
          List()
        )
    }

  val update: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      // === ELM Function ===
      // update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
      // update targetKey alter dictionary =
      // case alter(get targetKey dictionary) of
      //   Just value -> insert targetKey value dictionary
      //   Nothing    -> remove targetKey dictionary
      (evaluator: InvokeableEvaluator) => (
          targetKeyRaw: RTValue,
          alterRaw: RTValue,
          dictRaw: RTValue
      ) =>
        val dict      = dictRaw.coerceMap.value.clone() // make sure to clone it to not modify original one
        val currValue = optionToMaybe(dict.get(targetKeyRaw))
        val newValue  = evaluator.handleApplyResult(Type.UType.Unit(()), alterRaw, currValue)

        newValue match {
          case RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Maybe:Just"), List(value)) =>
            dict += ((targetKeyRaw, value))
          case RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Maybe:Nothing"), _) =>
            dict.remove(targetKeyRaw)
          case _ =>
            throw new UnexpectedType(
              s"Morphir.SDK:Maybe:just value or Morphir.SDK:Maybe:nothing",
              newValue,
              hint = "Expected because this was returned from Dict.update native function"
            )
        }
        RTValue.Map(dict)
    }
  }

  val insert: SDKValue = SDKValue.SDKNativeFunction.fun3 {
    (key: RTValue, value: RTValue, m: RTValue) =>
      val map =
        m.coerceMap.value.clone() // because LinkedHashMap is mutable MAKE SURE TO CLONE IT before inserting things
      map += ((key, value))
      RTValue.Map(map)
  }

  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Dict:empty")     -> empty,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:filter")    -> filter,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")       -> get,
    FQName.fromString("Morphir.SDK:Dict:member")    -> member,
    FQName.fromString("Morphir.SDK:Dict:isEmpty")   -> isEmpty,
    FQName.fromString("Morphir.SDK:Dict:size")      -> size,
    FQName.fromString("Morphir.SDK:Dict:insert")    -> insert,
    FQName.fromString("Morphir.SDK:Dict:keys")      -> keys,
    FQName.fromString("Morphir.SDK:Dict:values")    -> values,
    FQName.fromString("Morphir.SDK:Dict:toList")    -> toList,
    FQName.fromString("Morphir.SDK:Dict:singleton") -> singleton,
    FQName.fromString("Morphir.SDK:Dict:update")    -> update,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")       -> get,
    FQName.fromString("Morphir.SDK:Dict:filter")    -> filter,
    FQName.fromString("Morphir.SDK:Dict:insert")    -> insert,
    FQName.fromString("Morphir.SDK:Dict:empty")     -> empty
  )
}

object SetSDK {
  val fromList: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
      val list = arg.coerceList.value
      RTValue.Set(list.to(mutable.LinkedHashSet))
    }
  val toList: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
      val set = arg.coerceSet.value
      RTValue.List(set.to(List))
    }
  val member: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (l: RTValue, r: RTValue) =>
      val setR = r.coerceSet.value
      RTValue.Primitive.Boolean(setR.contains(l))
    }
  val size: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
      RTValue.Primitive.Int(arg.coerceSet.value.size)
    }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Set:fromList") -> fromList,
    FQName.fromString("Morphir.SDK:Set:toList")   -> toList,
    FQName.fromString("Morphir.SDK:Set:member")   -> member,
    FQName.fromString("Morphir.SDK:Set:size")     -> size
  )
}

object BasicsSDK {
  val append: SDKValue = SDKValue.SDKNativeFunction.fun2((a: RTValue, b: RTValue) =>
    (a, b) match {
      case (RTValue.List(aElements), RTValue.List(bElements)) =>
        RTValue.List(aElements.appendedAll(bElements))
      case (RTValue.Primitive.String(a), RTValue.Primitive.String(b)) => RTValue.Primitive.String(a + b)
      case (other1, other2) =>
        throw new WrongArgumentTypes(s"Append must be called on two Lists or two Strings", other1, other2)
    }
  )
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Basics:append") -> append
  )
}

object StringSDK {
  val length: SDKValue =
    SDKValue.SDKNativeFunction.fun1((arg: RTValue) =>
      RTValue.Primitive.Int(arg.coerceString.value.length)
    )
  val append: SDKValue =
    SDKValue.SDKNativeFunction.fun2((a: RTValue, b: RTValue) =>
      RTValue.Primitive.String(a.coerceString.value + b.coerceString.value)
    )
  val left: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      RTValue.Primitive.String(
        b.coerceString.value.dropRight(b.coerceString.value.length - a.coerceInt.value.toInt)
      )
    }
  val right: SDKValue =
    SDKValue.SDKNativeFunction.fun2((a: RTValue, b: RTValue) =>
      RTValue.Primitive.String(b.coerceString.value.takeRight(a.coerceInt.value.toInt))
    )
  val fromInt: SDKValue = SDKValue.SDKNativeFunction.fun1((a: RTValue) =>
    // need to get unwrapInt.value which is an MInt, and then the value of that which is
    // a SafeLong (from Spire) and the print that.
    RTValue.Primitive.String(a.coerceInt.value.value.toString)
  )
  val fromFloat: SDKValue = SDKValue.SDKNativeFunction.fun1((a: RTValue) =>
    RTValue.Primitive.String(a.coerceFloat.value.toString)
  )
  val toInt: SDKValue = SDKValue.SDKNativeFunction.fun1 { (a: RTValue) =>
    val optional = a.coerceString.value.toIntOption
    optional match {
      case Some(value) => RTValue.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:just"),
          List(RTValue.Primitive.Int(value))
        )
      case None => RTValue.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:nothing"),
          List()
        )
    }
  }
  val isEmpty: SDKValue = SDKValue.SDKNativeFunction.fun1((a: RTValue) =>
    RTValue.Primitive.Boolean(a.coerceString.value.length == 0)
  )
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:String:append")    -> append,
    FQName.fromString("Morphir.SDK:String:length")    -> length,
    FQName.fromString("Morphir.SDK:String:left")      -> left,
    FQName.fromString("Morphir.SDK:String:right")     -> right,
    FQName.fromString("Morphir.SDK:String:fromInt")   -> fromInt,
    FQName.fromString("Morphir.SDK:String:fromFloat") -> fromFloat,
    FQName.fromString("Morphir.SDK:String:toInt")     -> toInt,
    FQName.fromString("Morphir.SDK:String:isEmpty")   -> isEmpty
  )
}

object TupleSDK {
  val first: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
    arg.coerceTuple.value.head
  }
  val second: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
    val length = arg.coerceTuple.value.length
    if (length < 2) {
      throw new UnexpectedType("(Tuple of at least two elements)", arg, "Expected because Tuple.second was called")
    }
    arg.coerceTuple.value(1)
  }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Tuple:first")  -> first,
    FQName.fromString("Morphir.SDK:Tuple:second") -> second
  )
}

object Native {
  private def handleSameNumerics(
      a: RTValue,
      b: RTValue
  )(f: (Any, Any, scala.Numeric[Any]) => Any): RTValue.Primitive[_] = {
    val components = RTValue.unwrapNumericsWithHelper[Any](a, b)
    RTValue.Primitive.makeOrFail[Any](f(components.a, components.b, components.helper))
  }

  private def handleBooleans(
      a: RTValue,
      b: RTValue
  )(f: (Boolean, Boolean) => Boolean) = {
    val aBool = a.coerceBoolean.value
    val bBool = b.coerceBoolean.value
    RTValue.Primitive.Boolean(f(aBool, bBool))
  }

  val and: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleBooleans(a, b)(_ && _)
    }
  val or: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleBooleans(a, b)(_ || _)
    }
  val not: SDKValue = SDKValue.SDKNativeFunction.fun1((a: RTValue) =>
    RTValue.Primitive.Boolean(!a.coerceBoolean.value)
  )
  val plus: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.plus(aNum, bNum) }
    }
  val subtract: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.minus(aNum, bNum) }
    }
  val divide: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) =>
        // scala.Numeric does not handle division, it's part of scala.Fractional or scala.Integral, need to get the right one
        val components        = RTValue.unwrapNumericsWithHelper[Any](a, b)
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
  val multiply: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.times(aNum, bNum) }
    }

  val round: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: RTValue) =>
      val unwrapped: Double = arg.coerceFloat.value
      // Math.round(float) returns an int but Math.round(double) returns a long
      // so we need to cast back into an int
      val rounded: Int = {
        val rounded: Long = Math.round(unwrapped)
        if (rounded.isValidInt) rounded.toInt
        else throw new IllegalValue(
          s"Error rounding ${unwrapped} into an Integer. It could not be casted into an it because it is too large or too small."
        )
      }
      RTValue.Primitive.Int(rounded)
    }

  val negate: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: RTValue) =>
      val components = RTValue.unwrapNumericWithHelper[Any](a)
      RTValue.Primitive.makeOrFail(components.helper.negate(components.value))
  }

  val toFloat: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: RTValue) =>
      def wrap(num: Double) = RTValue.Primitive.Float(num)
      RTValue.coerceNumeric(a) match {
        // if it's already a float, don't need to re-wrap it
        case float: Primitive.Float      => float
        case Primitive.Int(value)        => wrap(value.toDouble)
        case Primitive.BigDecimal(value) => wrap(value.toDouble)
      }
  }
  val log: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (a: RTValue, b: RTValue) =>
      val denominator = Math.log(a.coerceDouble.value)
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(b.coerceDouble.value) / denominator
      RTValue.Primitive.Float(asDouble)
  }

  val utc = java.time.ZoneId.of("UTC")

  def fromMillisecondsEpoch(millis: Long): java.time.LocalTime =
    java.time.Instant.ofEpochMilli(millis).atZone(utc).toLocalTime()

  def fromMillisecondsNanos(millis: Long): java.time.LocalTime =
    java.time.LocalTime.of(0, 0).plusNanos(millis * 1000000)

  val fromMilliseconds: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: RTValue) =>
      val millis = a.coerceInt.value.toLong
      val time   = fromMillisecondsEpoch(millis)
      RTValue.LocalTime(time)
  }

  val pi: SDKValue = SDKValue.SDKNativeValue(RTValue.Primitive.Float(scala.math.Pi))
  val e: SDKValue  = SDKValue.SDKNativeValue(RTValue.Primitive.Float(scala.math.E))

  val just: SDKConstructor    = SDKConstructor.Explicit(List(Type.variable("contents")))
  val nothing: SDKConstructor = SDKConstructor.Explicit(List())
  val ok: SDKConstructor      = SDKConstructor.Explicit(List(Type.variable("contents")))
  val err: SDKConstructor     = SDKConstructor.Explicit(List(Type.variable("contents")))
  val gt: SDKConstructor      = SDKConstructor.Explicit(List())
  val lt: SDKConstructor      = SDKConstructor.Explicit(List())
  val eq: SDKConstructor      = SDKConstructor.Explicit(List())

  val nativeCtors: Map[FQName, SDKConstructor] = Map(
    FQName.fromString("Morphir.SDK:Maybe:just")    -> just,
    FQName.fromString("Morphir.SDK:Maybe:nothing") -> nothing,
    FQName.fromString("Morphir.SDK:Result:ok")     -> ok,
    FQName.fromString("Morphir.SDK:Result:err")    -> err
  )

  val native: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Basics:and")                 -> and,
    FQName.fromString("Morphir.SDK:Basics:or")                  -> or,
    FQName.fromString("Morphir.SDK:Basics:not")                 -> not,
    FQName.fromString("Morphir.SDK:Basics:pi")                  -> pi,
    FQName.fromString("Morphir.SDK:Basics:e")                   -> e,
    FQName.fromString("Morphir.SDK:Basics:add")                 -> plus,
    FQName.fromString("Morphir.SDK:Basics:subtract")            -> subtract,
    FQName.fromString("Morphir.SDK:Basics:divide")              -> divide,
    FQName.fromString("Morphir.SDK:Basics:multiply")            -> multiply,
    FQName.fromString("Morphir.SDK:Basics:round")               -> round,
    FQName.fromString("Morphir.SDK:Basics:negate")              -> negate,
    FQName.fromString("Morphir.SDK:Basics:toFloat")             -> toFloat,
    FQName.fromString("Morphir.SDK:Basics:logBase")             -> log,
    FQName.fromString("Morphir.SDK:Decimal:minusOne")           -> minusOne,
    FQName.fromString("Morphir.SDK:Decimal:one")                -> one,
    FQName.fromString("Morphir.SDK:Decimal:zero")               -> zero,
    FQName.fromString("Morphir.SDK:Aggregate:count")            -> count,
    FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds") -> fromMilliseconds
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  ) ++ DictSDK.sdk ++ SetSDK.sdk ++ StringSDK.sdk ++ SetSDK.sdk ++ TupleSDK.sdk ++ BasicsSDK.sdk
}

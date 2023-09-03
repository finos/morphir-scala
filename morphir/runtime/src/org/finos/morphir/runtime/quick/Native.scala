package org.finos.morphir.runtime.quick

import org.finos.morphir.extensibility.SdkModuleDescriptors.Morphir
import org.finos.morphir.ir.Type

import org.finos.morphir.naming._
import org.finos.morphir.runtime.{IllegalValue, UnexpectedType, UnsupportedType}
import org.finos.morphir.runtime.quick.Result.Primitive
import org.finos.morphir.runtime.Extractors._
import scala.collection.mutable
import org.finos.morphir.runtime.UnsupportedType
import org.finos.morphir.runtime.quick.Result.Primitive
import scala.collection.mutable

object DictSDK {
  // === ELM Function ===
  // filter : (k -> v -> Bool) -> Dict k v -> Dict comparable v
  // filter isGood dict =
  //   foldl (\k v d -> if isGood k v then insert k v d else d) empty dict
  val filter: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: Loop[Unit, Type.UType]) => (isGood: Result[Unit, Type.UType], dictRaw: Result[Unit, Type.UType]) =>
        {
          val dictMap = dictRaw.unwrapMap
          val newDict =
            dictMap.filter { case (k, v) =>
              val result = evaluator.handleApplyResult2(Type.UType.Unit(()), isGood, k, v)
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
          // unwrap the element that is in the list provided to the Dict.fromList function. It has to be a Tuple2
          input.unwrapTuple match {
            case List(a, b) => (a, b)
            case _ =>
              throw new IllegalValue(s"Input to Dict.fromList was not a Tuple2-based element, it was: `$input`")
          }
        }
      Result.MapResult(mutable.LinkedHashMap(mappedList: _*))
  }

  val toList: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 {
    (d: Result[Unit, Type.UType]) =>
      val dict     = d.unwrapMap
      val elements = dict.toList.map { case (k, v) => Result.Tuple(k, v) }
      Result.ListResult(elements)
  }

  val empty: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(Result.MapResult(mutable.LinkedHashMap()))

  val get: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (key: Result[Unit, Type.UType], m: Result[Unit, Type.UType]) =>
      val map = m.unwrapMap
      optionToMaybe(map.get(key))
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
      // === ELM Function ===
      // update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
      // update targetKey alter dictionary =
      // case alter(get targetKey dictionary) of
      //   Just value -> insert targetKey value dictionary
      //   Nothing    -> remove targetKey dictionary
      (evaluator: Loop[Unit, Type.UType]) => (
          targetKeyRaw: Result[Unit, Type.UType],
          alterRaw: Result[Unit, Type.UType],
          dictRaw: Result[Unit, Type.UType]
      ) =>
        val dict      = dictRaw.unwrapMap.clone() // make sure to clone it to not modify original one
        val currValue = optionToMaybe(dict.get(targetKeyRaw))
        val newValue  = evaluator.handleApplyResult(Type.UType.Unit(()), alterRaw, currValue)

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
    FQName.fromString("Morphir.SDK:Dict:update")    -> update,
    FQName.fromString("Morphir.SDK:Dict:fromList")  -> fromList,
    FQName.fromString("Morphir.SDK:Dict:get")       -> get,
    FQName.fromString("Morphir.SDK:Dict:filter")    -> filter,
    FQName.fromString("Morphir.SDK:Dict:insert")    -> insert,
    FQName.fromString("Morphir.SDK:Dict:empty")     -> empty
  )
}

object ListSDK {
  val any: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      // === ELM ===
      // -- NOTE: This is equivalent of Scala's List.exists function
      // any : (a -> Bool) -> List a -> Bool
      // any isOkay list =
      //   case list of
      //     [] -> False
      //     x :: xs -> if isOkay x then True else any isOkay xs
      (evaluator: Loop[Unit, Type.UType]) => (isOkay: Result[Unit, Type.UType], listRaw: Result[Unit, Type.UType]) =>
        {
          val list = listRaw.unwrapList
          val output =
            list.exists(elem =>
              evaluator.handleApplyResult(Type.UType.Unit(()), isOkay, elem).unwrapBoolean
            )
          Result.Primitive.Boolean(output)
        }
    }
  }

  val partition: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      // === ELM ===
      // -- The signature of the ELM function is this. This is equivalent of Scala's List.partition
      // partition : (a -> Bool) -> List a -> (List a, List a)
      // partition pred list = ...
      (evaluator: Loop[Unit, Type.UType]) => (pred: Result[Unit, Type.UType], listRaw: Result[Unit, Type.UType]) =>
        {
          val list = listRaw.unwrapList
          val (left, right) =
            list.partition(elem =>
              evaluator.handleApplyResult(Type.UType.Unit(()), pred, elem).unwrapBoolean
            )
          Result.Tuple[Unit, Type.UType](
            Result.ListResult[Unit, Type.UType](left),
            Result.ListResult[Unit, Type.UType](right)
          )
        }
    }
  }

  val foldl: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      (evaluator: Loop[Unit, Type.UType]) =>
        (f: Result[Unit, Type.UType], first: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
          {
            val list = l.unwrapList
            list.foldLeft(first) { (b, a) => // Note that elm does (a, b) => b, scala does it in the opposite way
              evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, b)
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
    FQName.fromString("Morphir.SDK:List:foldl")     -> foldl,
    FQName.fromString("Morphir.SDK:List:append")    -> append,
    FQName.fromString("Morphir.SDK:List:cons")      -> cons,
    FQName.fromString("Morphir.SDK:List:isEmpty")   -> isEmpty,
    FQName.fromString("Morphir.SDK:List:any")       -> any,
    FQName.fromString("Morphir.SDK:List:partition") -> partition
  )
}

object SetSDK {
  val fromList: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
      val list = arg.asInstanceOf[Result.ListResult[Unit, Type.UType]].elements
      Result.SetResult(list.to(mutable.LinkedHashSet))
    }
  val toList: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
      val set = arg.asInstanceOf[Result.SetResult[Unit, Type.UType]].elements
      Result.ListResult(set.to(List))
    }
  val member: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (l: Result[Unit, Type.UType], r: Result[Unit, Type.UType]) =>
      val setR = r.asInstanceOf[Result.SetResult[Unit, Type.UType]].elements
      Result.Primitive.Boolean(setR.contains(l))
    }
  val size: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
      Result.Primitive.Int(arg.asInstanceOf[Result.SetResult[Unit, Type.UType]].elements.size)
    }
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Set:fromList") -> fromList,
    FQName.fromString("Morphir.SDK:Set:toList")   -> toList,
    FQName.fromString("Morphir.SDK:Set:member")   -> member,
    FQName.fromString("Morphir.SDK:Set:size")     -> size
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
  val fromInt: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1((a: Result[Unit, Type.UType]) =>
    Result.Primitive.String(a.unwrapInt.toString)
  )
  val fromFloat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1((a: Result[Unit, Type.UType]) =>
    Result.Primitive.String(a.unwrapFloat.toString)
  )
  val toInt: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (a: Result[Unit, Type.UType]) =>
    val optional = a.unwrapString.toIntOption
    optional match {
      case Some(value) => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:just"),
          List(Result.Primitive.Int[Unit, Type.UType](value))
        )
      case None => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:nothing"),
          List()
        )
    }
  }
  val isEmpty: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1((a: Result[Unit, Type.UType]) =>
    Result.Primitive.Boolean(a.unwrapString.length == 0)
  )
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
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

object DecimalSDK {
  val fromFloat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
    Result.Primitive.BigDecimal(BigDecimal.valueOf(arg.unwrapFloat))
  }
  val toFloat: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
    Result.Primitive.Float(arg.unwrapDecimal.toDouble)
  }
  val asString: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
    Result.Primitive.String(arg.unwrapDecimal.toString)
  }
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Decimal:fromFloat") -> fromFloat,
    FQName.fromString("Morphir.SDK:Decimal:toFloat")   -> toFloat,
    FQName.fromString("Morphir.SDK:Decimal:toString")  -> asString
  )
}

object TupleSDK {
  val first: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
    arg.unwrapTuple.toList.head
  }
  val second: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun1 { (arg: Result[Unit, Type.UType]) =>
    val length = arg.unwrapTuple.toList.length
    if (length < 2) {
      throw new IllegalValue(s"Tuple with length `$length` has too few elements")
    }
    arg.unwrapTuple.toList(1)
  }
  val sdk: Map[FQName, SDKValue[Unit, Type.UType]] = Map(
    FQName.fromString("Morphir.SDK:Tuple:first")  -> first,
    FQName.fromString("Morphir.SDK:Tuple:second") -> second
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
      def wrap(num: Double) = Result.Primitive.Float[Unit, Type.UType](num)
      a.unwrapNumeric match {
        // if it's already a float, don't need to re-wrap it
        case float: Primitive.Float[_, _] => float
        case Primitive.Int(value)         => wrap(value.toDouble)
        case Primitive.Long(value)        => wrap(value.toDouble)
        case Primitive.BigDecimal(value)  => wrap(value.toDouble)
      }
  }
  val log: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeFunction.fun2 {
    (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      val denominator = Math.log(a.unwrapDouble)
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(b.unwrapDouble) / denominator
      Result.Primitive.Float(asDouble)
  }

  val lessThan: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.lt(a, b) }
    }

  val lessThanOrEqual: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.lteq(a, b) }
    }

  val greaterThan: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.gt(a, b) }
    }

  val greaterThanOrEqual: SDKValue[Unit, Type.UType] =
    SDKValue.SDKNativeFunction.fun2 { (a: Result[Unit, Type.UType], b: Result[Unit, Type.UType]) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.gteq(a, b) }
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
      (evaluator: Loop[Unit, Type.UType]) => (f: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
        val list = l.unwrapList
        val out =
          list.filter(elem =>
            evaluator.handleApplyResult(Type.UType.Unit(()), f, elem).unwrapBoolean
          )
        Result.ListResult(out)
    }
  }

  val map: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: Loop[Unit, Type.UType]) => (f: Result[Unit, Type.UType], l: Result[Unit, Type.UType]) =>
        val list = l.unwrapList
        val out =
          list.map(elem =>
            evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
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
      val millis = a.unwrapLong
      val time   = fromMillisecondsEpoch(millis)
      Result.LocalTime(time)
  }

  val pi: SDKValue[Unit, Type.UType] = SDKValue.SDKNativeValue(Result.Primitive.Float(3.toDouble))

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
    FQName.fromString("Morphir.SDK:Basics:lessThanOrEqual")     -> lessThanOrEqual,
    FQName.fromString("Morphir.SDK:Basics:greaterThanOrEqual")  -> greaterThanOrEqual,
    FQName.fromString("Morphir.SDK:List:concat")                -> concat,
    FQName.fromString("Morphir.SDK:List:singleton")             -> singleton,
    FQName.fromString("Morphir.SDK:List:isEmpty")               -> isEmpty,
    FQName.fromString("Morphir.SDK:List:map")                   -> map,
    FQName.fromString("Morphir.SDK:List:filter")                -> filter,
    FQName.fromString("Morphir.SDK:List:length")                -> length,
    FQName.fromString("Morphir.SDK:LocalDate:fromParts")        -> fromParts,
    FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds") -> fromMilliseconds
//    FQName.fromString("Morphir.Examples.App:Example:myMap") -> map
  ) ++ DictSDK.sdk ++ SetSDK.sdk ++ StringSDK.sdk ++ ListSDK.sdk ++ SetSDK.sdk ++ DecimalSDK.sdk ++ TupleSDK.sdk ++ BasicsSDK.sdk
}

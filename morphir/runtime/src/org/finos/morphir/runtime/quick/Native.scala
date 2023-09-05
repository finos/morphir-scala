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
  val filter: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: Loop) => (isGood: Result, dictRaw: Result) =>
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

  val fromList: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: Result) =>
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

  val toList: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (d: Result) =>
      val dict     = d.unwrapMap
      val elements = dict.toList.map { case (k, v) => Result.Tuple(k, v) }
      Result.ListResult(elements)
  }

  val empty: SDKValue = SDKValue.SDKNativeValue(Result.MapResult(mutable.LinkedHashMap()))

  val get: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (key: Result, m: Result) =>
      val map = m.unwrapMap
      optionToMaybe(map.get(key))
  }

  val singleton: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (key: Result, value: Result) =>
      Result.MapResult(mutable.LinkedHashMap.from(List(key -> value)))
  }

  val keys: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (m: Result) =>
      val map = m.unwrapMap
      Result.ListResult(map.keys.toList)
  }

  private def optionToMaybe(opt: Option[Result]): Result =
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

  val update: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      // === ELM Function ===
      // update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
      // update targetKey alter dictionary =
      // case alter(get targetKey dictionary) of
      //   Just value -> insert targetKey value dictionary
      //   Nothing    -> remove targetKey dictionary
      (evaluator: Loop) => (
          targetKeyRaw: Result,
          alterRaw: Result,
          dictRaw: Result
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

  val insert: SDKValue = SDKValue.SDKNativeFunction.fun3 {
    (key: Result, value: Result, m: Result) =>
      val map = m.unwrapMap.clone() // because LinkedHashMap is mutable MAKE SURE TO CLONE IT before inserting things
      map += ((key, value))
      Result.MapResult(map)
  }

  val sdk: Map[FQName, SDKValue] = Map(
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
  val any: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      // === ELM ===
      // -- NOTE: This is equivalent of Scala's List.exists function
      // any : (a -> Bool) -> List a -> Bool
      // any isOkay list =
      //   case list of
      //     [] -> False
      //     x :: xs -> if isOkay x then True else any isOkay xs
      (evaluator: Loop) => (isOkay: Result, listRaw: Result) =>
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

  val partition: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      // === ELM ===
      // -- The signature of the ELM function is this. This is equivalent of Scala's List.partition
      // partition : (a -> Bool) -> List a -> (List a, List a)
      // partition pred list = ...
      (evaluator: Loop) => (pred: Result, listRaw: Result) =>
        {
          val list = listRaw.unwrapList
          val (left, right) =
            list.partition(elem =>
              evaluator.handleApplyResult(Type.UType.Unit(()), pred, elem).unwrapBoolean
            )
          Result.Tuple(
            Result.ListResult(left),
            Result.ListResult(right)
          )
        }
    }
  }

  val foldl: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun3 {
      (evaluator: Loop) => (f: Result, first: Result, l: Result) =>
        {
          val list = l.unwrapList
          list.foldLeft(first) { (b, a) => // Note that elm does (a, b) => b, scala does it in the opposite way
            evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, b)
          }
        }
    }
  }
  val append: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (a: Result, b: Result) =>
      val listA = a.asInstanceOf[Result.ListResult]
      val listB = b.asInstanceOf[Result.ListResult]
      Result.ListResult(listA.elements.appendedAll(listB.elements))
  }
  val cons: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (a: Result, b: Result) =>
      val listB = b.asInstanceOf[Result.ListResult]
      Result.ListResult(a :: listB.elements)
  }
  val isEmpty: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
      val listA = arg.asInstanceOf[Result.ListResult].elements
      Result.Primitive.Boolean(listA.size == 0)
    }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:List:foldl")     -> foldl,
    FQName.fromString("Morphir.SDK:List:append")    -> append,
    FQName.fromString("Morphir.SDK:List:cons")      -> cons,
    FQName.fromString("Morphir.SDK:List:isEmpty")   -> isEmpty,
    FQName.fromString("Morphir.SDK:List:any")       -> any,
    FQName.fromString("Morphir.SDK:List:partition") -> partition
  )
}

object SetSDK {
  val fromList: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
      val list = arg.asInstanceOf[Result.ListResult].elements
      Result.SetResult(list.to(mutable.LinkedHashSet))
    }
  val toList: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
      val set = arg.asInstanceOf[Result.SetResult].elements
      Result.ListResult(set.to(List))
    }
  val member: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (l: Result, r: Result) =>
      val setR = r.asInstanceOf[Result.SetResult].elements
      Result.Primitive.Boolean(setR.contains(l))
    }
  val size: SDKValue =
    SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
      Result.Primitive.Int(arg.asInstanceOf[Result.SetResult].elements.size)
    }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Set:fromList") -> fromList,
    FQName.fromString("Morphir.SDK:Set:toList")   -> toList,
    FQName.fromString("Morphir.SDK:Set:member")   -> member,
    FQName.fromString("Morphir.SDK:Set:size")     -> size
  )
}

object BasicsSDK {
  val append: SDKValue = SDKValue.SDKNativeFunction.fun2((a: Result, b: Result) =>
    (a, b) match {
      case (Result.ListResult(aElements), Result.ListResult(bElements)) =>
        Result.ListResult(aElements.appendedAll(bElements))
      case (Result.Primitive.String(a), Result.Primitive.String(b)) => Result.Primitive.String(a + b)
      case (other1, other2) => throw new UnsupportedType(s"Append called on unrecognized types: $other1, $other2")
    }
  )
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Basics:append") -> append
  )
}

object StringSDK {
  val length: SDKValue =
    SDKValue.SDKNativeFunction.fun1((arg: Result) =>
      Result.Primitive.Int(arg.unwrapString.length)
    )
  val append: SDKValue =
    SDKValue.SDKNativeFunction.fun2((a: Result, b: Result) =>
      Result.Primitive.String(a.unwrapString + b.unwrapString)
    )
  val left: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      Result.Primitive.String(
        b.unwrapString.dropRight(b.unwrapString.length - a.unwrapInt)
      )
    }
  val right: SDKValue =
    SDKValue.SDKNativeFunction.fun2((a: Result, b: Result) =>
      Result.Primitive.String(b.unwrapString.takeRight(a.unwrapInt))
    )
  val fromInt: SDKValue = SDKValue.SDKNativeFunction.fun1((a: Result) =>
    Result.Primitive.String(a.unwrapInt.toString)
  )
  val fromFloat: SDKValue = SDKValue.SDKNativeFunction.fun1((a: Result) =>
    Result.Primitive.String(a.unwrapFloat.toString)
  )
  val toInt: SDKValue = SDKValue.SDKNativeFunction.fun1 { (a: Result) =>
    val optional = a.unwrapString.toIntOption
    optional match {
      case Some(value) => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:just"),
          List(Result.Primitive.Int(value))
        )
      case None => Result.ConstructorResult(
          FQName.fromString("Morphir.SDK:Maybe:nothing"),
          List()
        )
    }
  }
  val isEmpty: SDKValue = SDKValue.SDKNativeFunction.fun1((a: Result) =>
    Result.Primitive.Boolean(a.unwrapString.length == 0)
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

object DecimalSDK {
  val fromFloat: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
    Result.Primitive.BigDecimal(BigDecimal.valueOf(arg.unwrapFloat))
  }
  val toFloat: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
    Result.Primitive.Float(arg.unwrapDecimal.toDouble)
  }
  val asString: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
    Result.Primitive.String(arg.unwrapDecimal.toString)
  }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Decimal:fromFloat") -> fromFloat,
    FQName.fromString("Morphir.SDK:Decimal:toFloat")   -> toFloat,
    FQName.fromString("Morphir.SDK:Decimal:toString")  -> asString
  )
}

object TupleSDK {
  val first: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
    arg.unwrapTuple.toList.head
  }
  val second: SDKValue = SDKValue.SDKNativeFunction.fun1 { (arg: Result) =>
    val length = arg.unwrapTuple.toList.length
    if (length < 2) {
      throw new IllegalValue(s"Tuple with length `$length` has too few elements")
    }
    arg.unwrapTuple.toList(1)
  }
  val sdk: Map[FQName, SDKValue] = Map(
    FQName.fromString("Morphir.SDK:Tuple:first")  -> first,
    FQName.fromString("Morphir.SDK:Tuple:second") -> second
  )
}

object Native {
  private def handleSameNumerics(
      a: Result,
      b: Result
  )(f: (Any, Any, scala.Numeric[Any]) => Any): Result.Primitive[_] = {
    val components = Result.unwrapNumericsWithHelper[Any](a, b)
    Result.Primitive.makeOrFail[Any](f(components.a, components.b, components.helper))
  }

  private def handleBooleans(
      a: Result,
      b: Result
  )(f: (Boolean, Boolean) => Boolean) = {
    val aBool = Result.unwrapBoolean(a)
    val bBool = Result.unwrapBoolean(b)
    Result.Primitive.Boolean(f(aBool, bBool))
  }

  val and: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleBooleans(a, b)(_ && _)
    }
  val or: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleBooleans(a, b)(_ || _)
    }
  val not: SDKValue = SDKValue.SDKNativeFunction.fun1((a: Result) =>
    Result.Primitive.Boolean(!a.unwrapBoolean)
  )
  val plus: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.plus(aNum, bNum) }
    }
  val subtract: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.minus(aNum, bNum) }
    }
  val divide: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) =>
        // scala.Numeric does not handle division, it's part of scala.Fractional or scala.Integral, need to get the right one
        val components        = Result.unwrapNumericsWithHelper[Any](a, b)
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
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (aNum, bNum, helper) => helper.times(aNum, bNum) }
    }
  val negate: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: Result) =>
      val components = Result.unwrapNumericWithHelper[Any](a)
      Result.Primitive.makeOrFail(components.helper.negate(components.value))
  }

  val toFloat: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: Result) =>
      def wrap(num: Double) = Result.Primitive.Float(num)
      a.unwrapNumeric match {
        // if it's already a float, don't need to re-wrap it
        case float: Primitive.Float      => float
        case Primitive.Int(value)        => wrap(value.toDouble)
        case Primitive.Long(value)       => wrap(value.toDouble)
        case Primitive.BigDecimal(value) => wrap(value.toDouble)
      }
  }
  val log: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (a: Result, b: Result) =>
      val denominator = Math.log(a.unwrapDouble)
      val asDouble =
        if (denominator == 0)
          Double.PositiveInfinity
        else
          Math.log(b.unwrapDouble) / denominator
      Result.Primitive.Float(asDouble)
  }

  val lessThan: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.lt(a, b) }
    }

  val lessThanOrEqual: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.lteq(a, b) }
    }

  val greaterThan: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.gt(a, b) }
    }

  val greaterThanOrEqual: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: Result, b: Result) =>
      handleSameNumerics(a, b) { (a, b, helper) => helper.gteq(a, b) }
    }

  val equal: SDKValue = SDKValue.SDKNativeFunction.fun2 {
    (a: Result, b: Result) =>
      Result.Primitive.Boolean(a == b)
  }

  val concat: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: Result) =>
      val list      = l.asInstanceOf[Result.ListResult].elements
      val flattened = list.flatMap(inner => inner.asInstanceOf[Result.ListResult].elements)
      Result.ListResult(flattened)
  }

  val singleton: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: Result) =>
      Result.ListResult(List(l))
  }

  val isEmpty: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: Result) =>
      val list = l.unwrapList
      Result.Primitive.Boolean(list.length == 0)
  }

  val length: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (l: Result) =>
      val list = l.unwrapList
      Result.Primitive.Int(list.length)
  }

  val filter: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: Loop) => (f: Result, l: Result) =>
        val list = l.unwrapList
        val out =
          list.filter(elem =>
            evaluator.handleApplyResult(Type.UType.Unit(()), f, elem).unwrapBoolean
          )
        Result.ListResult(out)
    }
  }

  val map: SDKValue = SDKValue.SDKNativeInnerFunction {
    NativeFunctionSignatureAdv.Fun2 {
      (evaluator: Loop) => (f: Result, l: Result) =>
        val list = l.unwrapList
        val out =
          list.map(elem =>
            evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          )
        Result.ListResult(out)
    }
  }

  val fromParts: SDKValue = SDKValue.SDKNativeFunction.fun3 {
    (a: Result, b: Result, c: Result) =>
      Result.LocalDate(java.time.LocalDate.of(a.unwrapInt, b.unwrapInt, c.unwrapInt))
  }

  val utc = java.time.ZoneId.of("UTC")

  def fromMillisecondsEpoch(millis: Long): java.time.LocalTime =
    java.time.Instant.ofEpochMilli(millis).atZone(utc).toLocalTime()

  def fromMillisecondsNanos(millis: Long): java.time.LocalTime =
    java.time.LocalTime.of(0, 0).plusNanos(millis * 1000000)

  val fromMilliseconds: SDKValue = SDKValue.SDKNativeFunction.fun1 {
    (a: Result) =>
      val millis = a.unwrapLong
      val time   = fromMillisecondsEpoch(millis)
      Result.LocalTime(time)
  }

  val pi: SDKValue = SDKValue.SDKNativeValue(Result.Primitive.Float(3.toDouble))

  val just: SDKConstructor    = SDKConstructor(List(Type.variable("contents")))
  val nothing: SDKConstructor = SDKConstructor(List())
  val ok: SDKConstructor      = SDKConstructor(List(Type.variable("contents")))
  val err: SDKConstructor     = SDKConstructor(List(Type.variable("contents")))

  val nativeCtors: Map[FQName, SDKConstructor] = Map(
    FQName.fromString("Morphir.SDK:Maybe:just")    -> just,
    FQName.fromString("Morphir.SDK:Maybe:nothing") -> nothing,
    FQName.fromString("Morphir.SDK:Result:ok")     -> ok,
    FQName.fromString("Morphir.SDK:Result:err")    -> err
  )

  val native: Map[FQName, SDKValue] = Map(
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

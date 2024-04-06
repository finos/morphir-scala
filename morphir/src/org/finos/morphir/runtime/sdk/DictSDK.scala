package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime._
import org.finos.morphir.runtime.internal._

object DictSDK {
  val partition = DynamicNativeFunction2("partition") {
    (ctx: NativeContext) => (pred: RTValue.Function, dict: RTValue.Map) =>
      {
        val (part1, part2) = dict.value.partition { case (k, v) =>
          ctx.evaluator.handleApplyResult2(Type.UType.Unit(()), pred, k, v)
            .coerceBoolean
            .value
        }

        RTValue.Tuple(RTValue.Map(part1), RTValue.Map(part2))
      }
  }

  val remove = DynamicNativeFunction2("remove") {
    (ctx: NativeContext) => (comp: RTValue, dict: RTValue.Map) =>
      {
        val newDict = dict.value.clone() // clone to avoid mutating the caller's dict
        newDict.remove(comp)
        RTValue.Map(newDict)
      }
  }

  val diff = DynamicNativeFunction2("diff") {
    // === ELM Function ===
    // diff : Dict comparable a -> Dict comparable b -> Dict comparable a
    // Keep a key-value pair when its key does not appear in the second dictionary.
    (ctx: NativeContext) => (dict1Raw: RTValue.Map, dict2Raw: RTValue.Map) =>
      {
        val dict1 = dict1Raw.value.clone()
        val dict2 = dict2Raw.value.clone()
        val diff  = dict1.filter { case (k, _) => !dict2.contains(k) }
        RTValue.Map(diff)
      }
  }

  val intersect = DynamicNativeFunction2("intersect") {
    // === ELM Function ===
    // intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
    // Keep a key-value pair when its key appears in the second dictionary. Preference is given to values in the first dictionary.
    (ctx: NativeContext) => (dict1Raw: RTValue.Map, dict2Raw: RTValue.Map) =>
      {
        val dict1 = dict1Raw.value.clone()
        val dict2 = dict2Raw.value.clone()
        val diff  = dict1.filter { case (k, _) => dict2.contains(k) }
        RTValue.Map(diff)
      }
  }

  val union = DynamicNativeFunction2("union") {
    // === ELM Function ===
    // union : Dict comparable v -> Dict comparable v -> Dict comparable v
    // Combine two dictionaries. If there is a collision, preference is given to the first dictionary.
    (ctx: NativeContext) => (dict1Raw: RTValue.Map, dict2Raw: RTValue.Map) =>
      {
        val dict1 = dict1Raw.value.clone()
        val dict2 = dict2Raw.value.clone()
        val diff  = dict2 ++ dict1
        RTValue.Map(diff)
      }
  }

  val foldl = DynamicNativeFunction3("foldl") {
    // === ELM Function ===
    // foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
    // Fold over the key-value pairs in a dictionary from lowest key to highest key.
    (ctx: NativeContext) => (f: RTValue, b: RTValue, dictRaw: RTValue.Map) =>
      {
        val dict = dictRaw.value.clone()
        val result = dict.foldLeft(b) {
          case (acc, (k, v)) =>
            ctx.evaluator.handleApplyResult3(Type.UType.Unit(()), f, k, v, acc)
        }
        result
      }
  }

  val foldr = DynamicNativeFunction3("foldr") {
    // === ELM Function ===
    // foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
    // Fold over the key-value pairs in a dictionary from highest key to lowest key.
    (ctx: NativeContext) => (f: RTValue, b: RTValue, dictRaw: RTValue.Map) =>
      {
        val dict = dictRaw.value.clone()
        val result = dict.foldRight(b) {
          case ((k, v), acc) =>
            ctx.evaluator.handleApplyResult3(Type.UType.Unit(()), f, k, v, acc)
        }
        result
      }
  }

  val map = DynamicNativeFunction2("map") {
    // === ELM Function ===
    // map : (k -> a -> b) -> Dict k a -> Dict k b
    // Apply a function to all values in a diction
    (ctx: NativeContext) => (f: RTValue, dictRaw: RTValue.Map) =>
      {
        val dict = dictRaw.value.clone()
        val newDict = dict.map {
          case (k, v) =>
            val newValue = ctx.evaluator.handleApplyResult2(Type.UType.Unit(()), f, k, v)
            (k, newValue)
        }
        RTValue.Map(newDict)
      }
  }

  val merge = DynamicNativeFunction6("merge") {
    (ctx: NativeContext) =>
      (f: RTValue, g: RTValue, h: RTValue, dict1Raw: RTValue.Map, dict2Raw: RTValue.Map, result: RTValue) =>
        {
          val dict1 = dict1Raw.value.clone()
          val dict2 = dict2Raw.value.clone()
          val keys  = dict1.keySet ++ dict2.keySet
          val resultValue = keys.foldLeft(result) {
            case (acc, key) =>
              val value1 = dict1.get(key)
              val value2 = dict2.get(key)
              (value1, value2) match {
                case (Some(v1), Some(v2)) =>
                  ctx.evaluator.handleApplyResult4(Type.UType.Unit(()), g, key, v1, v2, acc)
                case (Some(v1), None) =>
                  ctx.evaluator.handleApplyResult3(Type.UType.Unit(()), f, key, v1, acc)
                case (None, Some(v2)) =>
                  ctx.evaluator.handleApplyResult3(Type.UType.Unit(()), h, key, v2, acc)
                case (None, None) =>
                  acc
              }
          }
          resultValue
        }
  }
}

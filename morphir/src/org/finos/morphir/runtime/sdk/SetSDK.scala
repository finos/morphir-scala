package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.RTValue.coerceComparable

import scala.collection.mutable

object SetSDK {

  val empty: SDKValue = SDKValue.SDKNativeValue(RTValue.Set(mutable.LinkedHashSet.empty))

  val foldr = DynamicNativeFunction3("foldr") {
    (context: NativeContext) => (f: RTValue.Function, zero: RTValue, set: RTValue.Set) =>
      val sortedSet = sortSet(set)
      // Elm foldr iterates on sort order, Scala iterates on insertion order.  Sort before folding.
      sortedSet.foldRight(zero) { (a, acc) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, acc)
      }
  }

  val foldl = DynamicNativeFunction3("foldl") {
    (context: NativeContext) => (f: RTValue.Function, zero: RTValue, set: RTValue.Set) =>
      val sortedSet = sortSet(set)
      // Elm foldl iterates on sort order, Scala iterates on insertion order.  Sort before folding.
      sortedSet.foldLeft(zero) { (acc, a) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, acc)
      }
  }

  private def sortSet(set: RTValue.Set): Set[RTValue] = set.value.toList.sortWith { (x, y) =>
    RTValue.Comparable.compareOrThrow(coerceComparable(x), coerceComparable(y)) < 0
  }.toSet

  val filter = DynamicNativeFunction2("filter") {
    (context: NativeContext) => (f: RTValue.Function, set: RTValue.Set) =>
      val result = set.elements.filter { elem =>
        val filterOutput = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
        RTValue.coerceBoolean(filterOutput).value
      }
      RTValue.Set(result)
  }

  val insert = DynamicNativeFunction2("insert") {
    (context: NativeContext) => (elem: RTValue, set: RTValue.Set) =>
      val newUnderlying = set.elements.clone() // copy the underlying mutable set to avoid modifying the original set
      newUnderlying.add(elem)
      RTValue.Set(newUnderlying)
  }

  val singleton = DynamicNativeFunction1("singleton") {
    (context: NativeContext) => (elem: RTValue) => RTValue.Set(mutable.LinkedHashSet(elem))
  }

  val union = DynamicNativeFunction2("union") {
    (context: NativeContext) => (setArg1: RTValue.Set, setArg2: RTValue.Set) =>
      RTValue.Set(setArg1.elements.union(setArg2.elements)) // union doesn't modify the contents of either set
  }

  val intersect = DynamicNativeFunction2("intersect") {
    (context: NativeContext) => (setArg1: RTValue.Set, setArg2: RTValue.Set) =>
      RTValue.Set(setArg1.elements.intersect(setArg2.elements)) // intersect doesn't modify the contents of either set
  }

  val diff = DynamicNativeFunction2("diff") {
    (context: NativeContext) => (setArg1: RTValue.Set, setArg2: RTValue.Set) =>
      RTValue.Set(setArg1.elements.diff(setArg2.elements)) // diff doesn't modify the contents of either set
  }

  val isEmpty = DynamicNativeFunction1("isEmpty") {
    (context: NativeContext) => (setArg: RTValue.Set) => RTValue.Primitive.Boolean(setArg.elements.isEmpty)
  }

  val map = DynamicNativeFunction2("map") {
    (context: NativeContext) => (f: RTValue.Function, set: RTValue.Set) =>
      val mappedUnderlying = set.elements.map(elem => context.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem))
      RTValue.Set(mappedUnderlying)
  }

  val partition = DynamicNativeFunction2("partition") {
    (context: NativeContext) => (pred: RTValue.Function, set: RTValue.Set) =>
      val (part1, part2) = set.elements.partition { elem =>
        context.evaluator.handleApplyResult(Type.UType.Unit(()), pred, elem)
          .coerceBoolean
          .value
      }

      RTValue.Tuple(RTValue.Set(part1), RTValue.Set(part2))
  }

  val remove = DynamicNativeFunction2("remove") {
    (context: NativeContext) => (elem: RTValue, set: RTValue.Set) =>
      val newUnderlying = set.elements.clone() // copy the underlying mutable set to avoid modifying the original set
      newUnderlying.remove(elem)
      RTValue.Set(newUnderlying)
  }
}

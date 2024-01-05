package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime._
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue

import scala.collection.mutable

object SetSDK {

  val foldr = DynamicNativeFunction3("foldr") {
    (context: NativeContext) => (f: RTValue.Function, zero: RTValue, set: RTValue.Set) =>
      // TODO: change foldr to sort before folding.  Currently blocked on comparable support
      // Elm foldr iterates on sort order, Scala iterates on insertion order.  Sort before folding.
      set.value.foldRight(zero) { (a, acc) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, acc)
      }
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

package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  NativeContext
}
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.{RTValue => RT}

object ListSDK {

  val concat = DynamicNativeFunction1("concat") {
    (context: NativeContext) => (list: RT.List) =>
      val flattened = list.elements.flatMap(inner => RT.coerceList(inner).elements)
      RT.List(flattened)
  }

  val singleton = DynamicNativeFunction1("singleton") {
    (context: NativeContext) => (l: RTValue) =>
      RT.List(List(l))
  }

  val isEmpty = DynamicNativeFunction1("isEmpty") {
    (context: NativeContext) => (list: RT.List) =>
      RT.Primitive.Boolean(list.elements.length == 0)
  }

  val length = DynamicNativeFunction1("length") {
    (context: NativeContext) => (list: RT.List) =>
      RT.Primitive.Int(list.elements.length)
  }

  val filter = DynamicNativeFunction2("filter") {
    (context: NativeContext) => (f: RT.Function, list: RT.List) =>
      val out =
        list.elements.filter { elem =>
          val filterOutput = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          RT.coerceBoolean(filterOutput).value
        }
      RT.List(out)
  }

  val map = DynamicNativeFunction2("map") {
    (ctx: NativeContext) => (f: RT.Function, listRaw: RT.List) =>
      {
        val out =
          listRaw.value.map(elem =>
            ctx.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          )
        RT.List(out)
      }
  }

  val any = DynamicNativeFunction2("any") {
    (context: NativeContext) =>
      // === ELM ===
      // -- NOTE: This is equivalent of Scala's List.exists function
      // any : (a -> Bool) -> List a -> Bool
      // any isOkay list =
      //   case list of
      //     [] -> False
      //     x :: xs -> if isOkay x then True else any isOkay xs
      (isOkay: RT.Function, listRaw: RT.List) =>
        {
          val out =
            listRaw.value.exists { elem =>
              val result = context.evaluator.handleApplyResult(Type.UType.Unit(()), isOkay, elem)
              RT.coerceBoolean(result).value
            }
          RT.Primitive.Boolean(out)
        }
  }

  val partition = DynamicNativeFunction2("partition") {
    (context: NativeContext) =>
      // === ELM ===
      // -- The signature of the ELM function is this. This is equivalent of Scala's List.partition
      // partition : (a -> Bool) -> List a -> (List a, List a)
      // partition pred list = ...
      (pred: RT.Function, listRaw: RT.List) =>
        {
          val (left, right) =
            listRaw.value.partition { elem =>
              val result = context.evaluator.handleApplyResult(Type.UType.Unit(()), pred, elem)
              RT.coerceBoolean(result).value
            }
          RT.Tuple(
            RT.List(left),
            RT.List(right)
          )
        }
  }

  val foldl = DynamicNativeFunction3("foldl") {
    (context: NativeContext) =>
      // === ELM ===
      // -- NOTE: This is equivalent of Scala's List.foldLeft function
      // foldl : (a -> b -> b) -> b -> List a -> b
      // foldl func first list
      (func: RT.Function, first: RTValue, listRaw: RT.List) =>
        {
          val out =
            listRaw.value.foldLeft(first) {
              (b, a) => // Note that elm does (a, b) => b, scala does it in the opposite way
                context.evaluator.handleApplyResult2(Type.UType.Unit(()), func, a, b)
            }
          out
        }
  }

  val append = DynamicNativeFunction2("append") {
    (context: NativeContext) => (a: RT.List, b: RT.List) =>
      RT.List(a.elements.appendedAll(b.elements))
  }

  val cons = DynamicNativeFunction2("cons") {
    (context: NativeContext) => (a: RTValue, listB: RT.List) =>
      RT.List(a :: listB.elements)
  }

  val all = DynamicNativeFunction2("all") {
    (context: NativeContext) => (predicate: RTValue.Function, list: RT.List) =>
      {
        val result = list.elements.forall { elem =>
          context.evaluator.handleApplyResult(Type.UType.Unit(()), predicate, elem)
            .coerceBoolean
            .value
        }
        RT.Primitive.Boolean(result)
      }
  }
}

package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime._
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue

object ListSDK {

  val concat = DynamicNativeFunction1("concat") {
    (context: NativeContext) => (list: RTValue.List) =>
      val flattened = list.elements.flatMap(inner => RTValue.coerceList(inner).elements)
      RTValue.List(flattened)
  }

  val singleton = DynamicNativeFunction1("singleton") {
    (context: NativeContext) => (l: RTValue) =>
      RTValue.List(List(l))
  }

  val isEmpty = DynamicNativeFunction1("isEmpty") {
    (context: NativeContext) => (list: RTValue.List) =>
      RTValue.Primitive.Boolean(list.elements.length == 0)
  }

  val length = DynamicNativeFunction1("length") {
    (context: NativeContext) => (list: RTValue.List) =>
      RTValue.Primitive.Int(list.elements.length)
  }

  val filter = DynamicNativeFunction2("filter") {
    (context: NativeContext) => (f: RTValue.Function, list: RTValue.List) =>
      val out =
        list.elements.filter { elem =>
          val filterOutput = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          RTValue.coerceBoolean(filterOutput).value
        }
      RTValue.List(out)
  }

  val map = DynamicNativeFunction2("map") {
    (ctx: NativeContext) => (f: RTValue.Function, listRaw: RTValue.List) =>
      {
        val out =
          listRaw.value.map(elem =>
            ctx.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          )
        RTValue.List(out)
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
      (isOkay: RTValue.Function, listRaw: RTValue.List) =>
        {
          val out =
            listRaw.value.exists { elem =>
              val result = context.evaluator.handleApplyResult(Type.UType.Unit(()), isOkay, elem)
              RTValue.coerceBoolean(result).value
            }
          RTValue.Primitive.Boolean(out)
        }
  }

  val partition = DynamicNativeFunction2("partition") {
    (context: NativeContext) =>
      // === ELM ===
      // -- The signature of the ELM function is this. This is equivalent of Scala's List.partition
      // partition : (a -> Bool) -> List a -> (List a, List a)
      // partition pred list = ...
      (pred: RTValue.Function, listRaw: RTValue.List) =>
        {
          val (left, right) =
            listRaw.value.partition { elem =>
              val result = context.evaluator.handleApplyResult(Type.UType.Unit(()), pred, elem)
              RTValue.coerceBoolean(result).value
            }
          RTValue.Tuple(
            RTValue.List(left),
            RTValue.List(right)
          )
        }
  }

  val foldl = DynamicNativeFunction3("foldl") {
    (context: NativeContext) =>
      // === ELM ===
      // -- NOTE: This is equivalent of Scala's List.foldLeft function
      // foldl : (a -> b -> b) -> b -> List a -> b
      // foldl func first list
      (func: RTValue.Function, first: RTValue, listRaw: RTValue.List) =>
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
    (context: NativeContext) => (a: RTValue.List, b: RTValue.List) =>
      RTValue.List(a.elements.appendedAll(b.elements))
  }

  val cons = DynamicNativeFunction2("cons") {
    (context: NativeContext) => (a: RTValue, listB: RTValue.List) =>
      RTValue.List(a :: listB.elements)
  }

  val all = DynamicNativeFunction2("all") {
    (context: NativeContext) => (predicate: RTValue.Function, list: RTValue.List) =>
      {
        val result = list.elements.forall { elem =>
          context.evaluator.handleApplyResult(Type.UType.Unit(()), predicate, elem)
            .coerceBoolean
            .value
        }
        RTValue.Primitive.Boolean(result)
      }
  }

  val concatMap = DynamicNativeFunction2("concatMap") {
    (ctx: NativeContext) => (f: RTValue.Function, listRaw: RTValue.List) =>
      {
        val out = listRaw.value.flatMap { elem =>
          val resultListRaw = ctx.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
          RTValue.coerceList(resultListRaw).elements
        }
        RTValue.List(out)
      }
  }

  val drop = DynamicNativeFunction2("drop") {
    (_: NativeContext) => (n: RTValue.Primitive.Int, list: RTValue.List) =>
      list.copy(elements = list.elements.drop(n.value.toInt))
  }

  val filterMap = DynamicNativeFunction2("filterMap") {
    (context: NativeContext) => (f: RTValue.Function, list: RTValue.List) =>
      val out = list.elements.map { elem =>
        val maybeOutputRaw = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, elem)
        val maybeOutputCr = RTValue.coerceConstructorResult(maybeOutputRaw)
        MaybeSDK.eitherToOption(maybeOutputCr)
      }.flatten
      RTValue.List(out)
  }
}

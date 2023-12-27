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
}

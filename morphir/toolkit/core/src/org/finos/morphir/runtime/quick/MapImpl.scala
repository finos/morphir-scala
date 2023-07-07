package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.{FQName, Name, Type}
import org.finos.morphir.ir.{Value => V}
import org.finos.morphir.ir.Value.*
import zio.Chunk

object MapImpl {

  def ir = {
    val inputTypes = Chunk(
      (Name("f"), Type.unit, Type.unit),
      (Name("list"), Type.unit, Type.unit)
    )
    val outputType = Type.unit
    val recurse = V.apply(
      V.apply(
        V.reference(FQName.fromString("Morphir.SDK:List:map")),
        V.variable("f")
      ),
      V.variable("tail")
    )
    val consExpr = V.apply(
      V.apply(
        V.reference(FQName.fromString("Morphir.SDK:List:cons")),
        V.apply(
          V.variable("f"),
          V.variable("head")
        )
      ),
      recurse
    )
    V.Definition(
      inputTypes,
      outputType,
      body = V.patternMatch(
        V.variable("list"),
        Chunk(
          (V.emptyListPattern, V.list()),
          (V.headTailPattern(V.asAlias("head"), V.asAlias("tail")), consExpr)
        )
      ) :> Type.unit
    )
  }
}

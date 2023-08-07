package org.finos.morphir.ir.sdk

import org.finos.morphir.naming._
import zio.Chunk
import org.finos.morphir.ir.Type.unit
import org.finos.morphir.ir.Value.Specification
import org.finos.morphir.ir.{Documented, Module}

object Regex extends MorphirIRSdkModule("Regex") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map.empty,
    values = toSpec(
      Chunk(
        "fromString",
        "fromStringWith",
        "never",
        "contains",
        "split",
        "find",
        "replace",
        "splitAtMost",
        "findAtMost",
        "replaceAtMost"
      )
    )
  )

  private def toSpec(values: Chunk[String]): Map[Name, Documented[Specification[Any]]] =
    values.map(valueName => (name(valueName), Documented("", Specification(Chunk.empty, unit)))).toMap
}

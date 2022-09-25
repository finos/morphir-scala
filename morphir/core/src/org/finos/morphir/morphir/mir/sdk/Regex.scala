package org.finos.morphir.mir.sdk

import zio.Chunk
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.Type.Type.unit
import org.finos.morphir.mir.Value.Specification
import org.finos.morphir.mir.{Documented, Module, Name}
import org.finos.morphir.syntax.NamingSyntax._

object Regex {
  val moduleName: ModuleName = ModuleName.fromString("Regex")

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

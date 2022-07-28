package morphir.ir.sdk

import zio.Chunk
import morphir.ir.Module.ModuleName
import morphir.ir.Type.Type.unit
import morphir.ir.Value.Specification
import morphir.ir.{Documented, Module, Name}
import morphir.syntax.NamingSyntax._

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

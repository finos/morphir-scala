package morphir.mir.sdk

import zio.Chunk
import morphir.mir.Module
import morphir.mir.Module.ModuleName
import morphir.mir.Type.Specification.CustomTypeSpecification
import morphir.mir.Type.Type._
import morphir.mir.Type.{Type, UConstructors, UType}
import morphir.mir.sdk.Common._
import morphir.syntax.NamingSyntax._

object Month {
  val moduleName: ModuleName = ModuleName.fromString("Month")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Month")
        -> CustomTypeSpecification(
          Chunk.empty,
          UConstructors(
            Map(
              name("January")   -> Chunk.empty,
              name("February")  -> Chunk.empty,
              name("March")     -> Chunk.empty,
              name("April")     -> Chunk.empty,
              name("May")       -> Chunk.empty,
              name("June")      -> Chunk.empty,
              name("July")      -> Chunk.empty,
              name("August")    -> Chunk.empty,
              name("September") -> Chunk.empty,
              name("October")   -> Chunk.empty,
              name("November")  -> Chunk.empty,
              name("December")  -> Chunk.empty
            )
          )
        )
        ?? "Type that represents an month concept."
    ),
    values = Map.empty
  )

  lazy val dateType: UType                = reference(toFQName(moduleName, "Month"))
  def dateType[A](attributes: A): Type[A] = reference(attributes, toFQName(moduleName, "Month"))
}

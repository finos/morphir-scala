package morphir.ir.sdk

import zio.Chunk
import morphir.ir.Module
import morphir.ir.Module.ModuleName
import morphir.ir.Type.Constructors
import morphir.ir.Type.Specification.CustomTypeSpecification
import morphir.ir.Type.Type._
import morphir.ir.sdk.Maybe.maybeType
import morphir.syntax.NamingSyntax._

object StatefulApp {
  val moduleName: ModuleName = ModuleName.fromString("StatefulApp")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("StatefulApp") -> CustomTypeSpecification(
        Chunk(name("k"), name("c"), name("s"), name("e")),
        Constructors(
          Map(
            name("StatefulApp") -> Chunk(
              (
                name("logic"),
                function(
                  maybeType(variable(name("s"))),
                  function(
                    variable(name("c")),
                    tuple(
                      Chunk(
                        maybeType(variable(name("s"))),
                        variable(name("e"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) ?? "Type that represents a stateful app."
    ),
    values = Map.empty
  )
}

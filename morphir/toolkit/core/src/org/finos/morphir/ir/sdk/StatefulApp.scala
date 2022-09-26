package org.finos.morphir.ir.sdk

import zio.Chunk
import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Module.ModuleName
import org.finos.morphir.ir.Type.Constructors
import org.finos.morphir.ir.Type.Specification.CustomTypeSpecification
import org.finos.morphir.ir.Type.Type._
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.syntax.NamingSyntax._

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

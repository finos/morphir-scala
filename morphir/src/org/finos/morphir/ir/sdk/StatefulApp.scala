package org.finos.morphir.ir.sdk

import zio.Chunk
import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Constructors
import org.finos.morphir.ir.Type.Specification.CustomTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Maybe.maybeType

object StatefulApp extends MorphirIRSdkModule("StatefulApp") {

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
                      scala.List(
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

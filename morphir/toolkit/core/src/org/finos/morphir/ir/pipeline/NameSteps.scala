package org.finos.morphir.ir.pipeline

import zio.prelude.fx._
import io.lemonlabs.uri.Urn
import org.finos.morphir.ir.Name

trait NameSteps {
  def nameToUrn: Step[Name, Any, Nothing, Urn] = Step.fromArgs(name => Urn("local-name", name.toList.mkString("-")))
  // def urnToName: Step[Urn, Any, Nothing, Name] = Step.fromFunction(urn => Name(urn.path.split("-").toList))

  // Chunk[Path] >> Chunk[Json] >> Chunk[(MorphirIR, Json)] >> Chunk[Distribution] >> Chunk[Distribution] >> Chunk[Json] >> Chunk[Text]
}

package org.finos.morphir.model

import zio.json._

/// Represents the structure of a Morphir project.
final case class MorphirProject(
    name: String,
    sourceDirectory: String,
    exposedModules: Set[String],
    localDependencies: Set[String] = Set.empty
)

object MorphirProject {
  implicit val decoder: JsonDecoder[MorphirProject] = DeriveJsonDecoder.gen[MorphirProject]
  implicit val encoder: JsonEncoder[MorphirProject] = DeriveJsonEncoder.gen[MorphirProject]
}

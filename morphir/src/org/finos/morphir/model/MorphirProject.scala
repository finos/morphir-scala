package org.finos.morphir.model

import zio.json._

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

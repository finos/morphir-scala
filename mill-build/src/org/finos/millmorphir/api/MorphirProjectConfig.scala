package org.finos.millmorphir.api

final case class MorphirProjectConfig(
    name: String,
    sourceDirectory: String,
    exposedModules: List[String],
    dependencies: List[String],
    localDependencies: List[String]
)

object MorphirProjectConfig {
  implicit val jsonFormatter: upickle.default.ReadWriter[MorphirProjectConfig] = upickle.default.macroRW
}

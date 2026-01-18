package org.finos.millmorphir.api

import upickle.default.*

final case class MorphirProjectConfig(
    name: String,
    sourceDirectory: String,
    exposedModules: List[String],
    dependencies: List[String],
    localDependencies: List[String]
) derives ReadWriter

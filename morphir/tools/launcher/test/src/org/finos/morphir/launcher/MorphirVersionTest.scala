package org.finos.morphir.launcher

case class MorphirVersionTest(
    defaultVersion: String,
    versionFromEnv: Option[String],
    versionFromFile: Option[String]
) extends MorphirVersion

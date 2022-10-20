package org.finos.morphir.launcher

import scala.io.Source
import scala.util.Try

/**
 * Resolve the Morphir version.
 *
 * Order of precedence is
 *   - MORPHIR_VERSION env variable
 *   - contents of version file .morphir-version
 *   - the current project version from mill
 */
trait MorphirVersion {
  def defaultVersion: String
  def versionFromEnv: Option[String]
  def versionFromFile: Option[String]

  final def version: String = versionFromEnv.getOrElse(versionFromFile.getOrElse(defaultVersion))
}

// Avoid using, for instance, ZIO, for module implementation to keep dependencies minimal.
case object MorphirVersionLive extends MorphirVersion {
  val morphirVersionKey: String      = "MORPHIR_VERSION"
  val morphirVersionFilePath: String = ".morphir-version"

  override val defaultVersion: String          = BuildInfo.version
  override def versionFromEnv: Option[String]  = sys.env.get(morphirVersionKey)
  override def versionFromFile: Option[String] = Try(Source.fromFile(morphirVersionFilePath).mkString.trim).toOption
}

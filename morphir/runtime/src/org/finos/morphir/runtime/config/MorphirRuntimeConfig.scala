package org.finos.morphir.runtime.config
//import org.finos.morphir.util.vfile.VPath
import zio._

//TODO: Consider making diagnosticsOutputDir a Option[VPath]
final case class MorphirRuntimeConfig(diagnosticsOutputDir: Option[String])
object MorphirRuntimeConfig {
  val config: Config[MorphirRuntimeConfig] = Config.string("diagnosticsOutputDir").optional.map(MorphirRuntimeConfig(_))
}

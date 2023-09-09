package org.finos.morphir.config
import lang.elm.ElmConfig

final case class WorkspaceConfig(lang: LanguagesConfig)
object WorkspaceConfig {
  val default: WorkspaceConfig = WorkspaceConfig(LanguagesConfig.default)
}

final case class LanguagesConfig(
    elm: ElmConfig
)
object LanguagesConfig {
  val default: LanguagesConfig = LanguagesConfig(ElmConfig.default)
}

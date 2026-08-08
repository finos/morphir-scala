package org.finos.morphir.mill

import upickle.default.ReadWriter

final case class MorphirProjectConfig(
    name: String,
    sourceDirectory: String,
    exposedModules: List[String] = Nil,
    dependencies: List[String] = Nil,
    localDependencies: List[String] = Nil
) derives ReadWriter {
  def withLocalDependencies(paths: List[String]): MorphirProjectConfig =
    copy(localDependencies = paths)
}

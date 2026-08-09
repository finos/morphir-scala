package org.finos.morphir.mill

import mill.*
import upickle.default.ReadWriter

final case class GeneratedMorphirSources(sourceRoot: PathRef, inputSha256: String) derives ReadWriter

trait MorphirGeneratedSources extends Module {
  def generatedMorphirSources: T[GeneratedMorphirSources]
}

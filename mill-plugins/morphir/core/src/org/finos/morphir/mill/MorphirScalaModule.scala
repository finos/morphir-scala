package org.finos.morphir.mill

import mill.*
import mill.scalalib.ScalaModule

trait MorphirScalaModule extends ScalaModule with MorphirGeneratedSources {
  override def generatedSources: T[Seq[PathRef]] = Task {
    super.generatedSources() :+ generatedMorphirSources().sourceRoot
  }
}

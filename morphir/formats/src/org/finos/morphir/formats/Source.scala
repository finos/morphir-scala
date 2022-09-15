package org.finos.morphir.formats

import zio._

abstract class Source[+A] {
  def parseText(text: String): Task[A] // TODO: Create a source error type
}

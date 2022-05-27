package org.finos.morphir.knowledge.core
import zio.stream.ZStream

object SStream {
  def succeed(maybeState: Option[State]): SStream = ZStream.succeed(maybeState)
}

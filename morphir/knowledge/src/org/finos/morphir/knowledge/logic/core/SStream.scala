package org.finos.morphir.knowledge.logic.core
import zio.stream.ZStream

object SStream {
  def succeed(maybeState: Option[State]): SStream = ZStream.succeed(maybeState)
}

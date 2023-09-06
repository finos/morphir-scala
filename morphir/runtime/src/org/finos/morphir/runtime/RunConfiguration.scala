package org.finos.morphir.runtime

import org.finos.morphir.ir.Distribution
import org.finos.morphir.ir.Distribution.Distribution

sealed trait RunConfiguration {}

object RunConfiguration {
  final case class Standard(distributions: List[Distribution]) extends RunConfiguration
}

package org.finos.morphir.runtime
import org.finos.morphir.ir.distribution.Distribution

trait EvaluationLibraryPlatformSpecific {
  def loadDistribution(fileName: String): Distribution =
    ???
}

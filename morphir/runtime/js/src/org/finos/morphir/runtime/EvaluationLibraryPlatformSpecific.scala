package org.finos.morphir.runtime

trait EvaluationLibraryPlatformSpecific {
  def loadDistribution(fileName: String): Distribution =
    ???
}

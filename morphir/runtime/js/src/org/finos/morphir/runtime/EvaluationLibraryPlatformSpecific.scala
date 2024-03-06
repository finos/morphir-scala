package org.finos.morphir.runtime
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.MorphirIRFile
import scala.io.Source

trait EvaluationLibraryPlatformSpecific {
  def loadDistribution(fileName: String): Distribution = {
    throw new Exception("Unsupported on this platform")
  }
}

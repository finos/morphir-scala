package org.finos.morphir.runtime
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.MorphirIRFile
import scala.io.Source

trait EvaluationLibraryPlatformSpecific {
  def loadDistribution(fileName: String): Distribution = {
    val text = Source
      .fromFile(fileName)
      .getLines()
      .mkString("\n")
    val morphirIRFile = text.fromJson[MorphirIRFile]
    morphirIRFile
      .getOrElse(throw new Exception(s"Failed to load $fileName as distribution"))
      .distribution
  }
}

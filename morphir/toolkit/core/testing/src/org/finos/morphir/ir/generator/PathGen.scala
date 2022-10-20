package org.finos.morphir.ir.generator

import org.finos.morphir.ir.Path
import org.finos.morphir.testing.generators.WordGen
import zio._
import zio.test.Gen

object PathGen extends PathGen

trait PathGen {
  val path: Gen[Any, Path] =
    Gen.listOfBounded(1, 5)(NameGen.name).map(parts => Path.fromList(parts))
}

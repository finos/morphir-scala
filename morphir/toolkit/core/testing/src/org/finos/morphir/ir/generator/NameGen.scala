package org.finos.morphir
package ir
package generator

import org.finos.morphir.testing.generators.WordGen
import zio._
import zio.test.Gen

object NameGen extends NameGen
trait NameGen {
  val name: Gen[Any, Name] =
    Gen.listOfBounded(1, 5)(WordGen.words).map(parts => Name.fromList(parts))
}

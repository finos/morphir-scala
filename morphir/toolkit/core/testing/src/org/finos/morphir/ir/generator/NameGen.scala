package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.testing.generators.WordGen
import zio.test.Gen

trait NameGen {
  final val name: Gen[Any, Name] = Gen.listOfBounded(1, 5)(WordGen.words).map(parts => Name.fromList(parts))
}

object NameGen extends NameGen

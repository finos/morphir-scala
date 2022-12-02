package org.finos.morphir
package ir
package generator

import zio.test.Gen

trait PathGen {
  final def path: Gen[Any, Path] = Gen.listOfBounded(1, 5)(NameGen.name).map(parts => Path.fromList(parts))
}

object PathGen extends PathGen

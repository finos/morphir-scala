package org.finos.morphir.ir.generator

import org.finos.morphir.ir.Path
import org.finos.morphir.ir.Module.ModulePath
import org.finos.morphir.testing.generators.WordGen
import zio._
import zio.test.Gen

object PathGen extends PathGen

trait PathGen {
  val modulePath = path.map(ModulePath(_))
  val path: Gen[Any, Path] =
    Gen.listOfBounded(1, 5)(NameGen.name).map(parts => Path.fromList(parts))

  def modulePath[R](path: Gen[R, Path]): Gen[R, ModulePath] = path.map(ModulePath(_))
}

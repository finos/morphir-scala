package org.finos.morphir.ir.generator

import org.finos.morphir.ir.Path
import org.finos.morphir.testing.generators.WordGen
import zio._
import zio.test.Gen
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._
import org.finos.morphir.ir.module.ModulePath

object PathDeriveGen extends PathDeriveGen
trait PathDeriveGen {
  implicit val pathDeriveGen: DeriveGen[Path] = DeriveGen.instance(PathGen.path)
  implicit def modulePathDeriveGen[R](implicit path: DeriveGen[Path]): DeriveGen[ModulePath] =
    DeriveGen.instance(PathGen.modulePath(path.derive))
}

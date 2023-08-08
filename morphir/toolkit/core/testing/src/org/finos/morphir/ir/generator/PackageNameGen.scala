package org.finos.morphir.ir.generator

import org.finos.morphir.naming._
import zio._
import zio.test.Gen

trait PackageNameGen {
  final val packageName: Gen[Any, PackageName] = PathGen.path.map(path => PackageName(path))
}

object PackageNameGen extends PackageNameGen

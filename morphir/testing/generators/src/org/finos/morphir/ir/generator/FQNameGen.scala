package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.Gen

trait FQNameGen {
  final def fqName[R](
      packagePathGen: Gen[R, Path],
      modulePathGen: Gen[R, Path],
      localNameGen: Gen[R, Name]
  ): Gen[R, FQName] =
    for {
      packagePath <- packagePathGen
      modulePath  <- modulePathGen
      name        <- localNameGen
    } yield FQName(PackageName(packagePath), ModuleName(modulePath), name)

  final val fqName: Gen[Any, FQName] = fqName(PathGen.path, PathGen.path, NameGen.name)
}

object FQNameGen extends FQNameGen

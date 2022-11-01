package org.finos.morphir
package ir
package generator

import zio._
import zio.test.Gen
trait FQNameGen {
  def fqName[R](packagePath: Gen[R, Path], modulePath: Gen[R, Path], localName: Gen[R, Name]): Gen[R, FQName] =
    for {
      packagePath <- packagePath
      modulePath  <- modulePath
      name        <- localName
    } yield FQName(packagePath, modulePath, name)

  val fqName: Gen[Any, FQName] = fqName(PathGen.path, PathGen.path, NameGen.name)
}

object FQNameGen extends FQNameGen

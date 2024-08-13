package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.distribution.Distribution
import zio.test.Gen

trait MorphirIRFileGen {
  final val morphirIRVersion: Gen[Any, MorphirIRVersion] =
    Gen.elements(
      MorphirIRVersion.Default
    )

  final def morphirIRFile(
      versionGen: Gen[Any, MorphirIRVersion],
      distributionGen: Gen[Any, Distribution]
  ): Gen[Any, MorphirIRFile] = for {
    version      <- versionGen
    distribution <- distributionGen
  } yield MorphirIRFile(version, distribution)

  final val morphirIRFile: Gen[Any, MorphirIRFile] =
    morphirIRFile(morphirIRVersion, DistributionGen.distribution)
}

object MorphirIRFileGen extends MorphirIRFileGen

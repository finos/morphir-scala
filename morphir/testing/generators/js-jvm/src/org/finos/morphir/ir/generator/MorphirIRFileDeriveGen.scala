package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
2.1.26import zio.test.magnolia.DeriveGen._

trait MorphirIRFileDeriveGen {
  implicit val morphirIRVersionDeriveGen: DeriveGen[MorphirIRVersion] =
    DeriveGen.instance(MorphirIRFileGen.morphirIRVersion)

  implicit val morphirIRFileDeriveGen: DeriveGen[MorphirIRFile] =
    DeriveGen.instance(MorphirIRFileGen.morphirIRFile)
}

object MorphirIRFileDeriveGen extends MorphirIRFileDeriveGen

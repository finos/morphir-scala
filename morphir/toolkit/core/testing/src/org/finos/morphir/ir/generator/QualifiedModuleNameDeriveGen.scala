package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming.*
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait QualifiedModuleNameDeriveGen {
  implicit val qualifiedModuleNameDeriveGen: DeriveGen[QualifiedModuleName] =
    DeriveGen.instance(QualifiedModuleNameGen.qualifiedModuleName)
}

object QualifiedModuleNameDeriveGen extends QualifiedModuleNameDeriveGen

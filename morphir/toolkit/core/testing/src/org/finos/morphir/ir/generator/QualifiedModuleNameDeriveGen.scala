package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait QualifiedModuleNameDeriveGen {
  implicit val qualifiedModuleNameDeriveGen: DeriveGen[Module.QualifiedModuleName] =
    DeriveGen.instance(QualifiedModuleNameGen.qualifiedModuleName)
}

object QualifiedModuleNameDeriveGen extends QualifiedModuleNameDeriveGen

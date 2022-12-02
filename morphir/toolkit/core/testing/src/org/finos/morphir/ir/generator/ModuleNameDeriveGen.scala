package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ModuleNameDeriveGen {
  implicit val moduleNameDeriveGen: DeriveGen[Module.ModuleName] = DeriveGen.instance(ModuleNameGen.moduleName)
}

object ModuleNameDeriveGen extends ModuleNameDeriveGen

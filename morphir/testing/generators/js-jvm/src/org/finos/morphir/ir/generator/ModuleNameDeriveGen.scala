package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
2.1.26import zio.test.magnolia.DeriveGen._

trait ModuleNameDeriveGen {
  implicit val moduleNameDeriveGen: DeriveGen[ModuleName] = DeriveGen.instance(ModuleNameGen.moduleName)
}

object ModuleNameDeriveGen extends ModuleNameDeriveGen

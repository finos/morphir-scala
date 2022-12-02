package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ModulePathDeriveGen {
  implicit val modulePathDeriveGen: DeriveGen[ModulePath] = DeriveGen.instance(ModulePathGen.modulePath)
}

object ModulePathDeriveGen extends ModulePathDeriveGen

package org.finos.morphir.ir.generator

import org.finos.morphir.ir.module.ModulePath
import zio._
import zio.test.Gen

trait ModulePathGen {
  final val modulePath: Gen[Any, ModulePath] = PathGen.path.map(path => ModulePath(path))
}

object ModulePathGen extends ModulePathGen

package org.finos.morphir.ir.generator

import org.finos.morphir.ir.module.ModuleName
import zio._
import zio.test.Gen

trait ModulePathGen {
  final val modulePath: Gen[Any, ModuleName] = PathGen.path.map(path => ModuleName(path))
}

object ModulePathGen extends ModulePathGen

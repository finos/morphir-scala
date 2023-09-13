package org.finos.morphir.ir.generator
import org.finos.morphir.naming.*

import zio._
import zio.test.Gen

trait ModuleNameGen {
  final val moduleName: Gen[Any, ModuleName] = PathGen.path.map(path => ModuleName(path))
}

object ModuleNameGen extends ModuleNameGen

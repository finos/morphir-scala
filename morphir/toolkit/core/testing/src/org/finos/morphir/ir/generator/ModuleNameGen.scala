package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.module.ModuleName
import zio.test.Gen

trait ModuleNameGen {
  final def moduleName[R](namespace: Gen[R, Path], localName: Gen[R, Name]): Gen[R, ModuleName] =
    for {
      namespace <- namespace
      localName <- localName
    } yield ModuleName(namespace, localName)

  final val moduleName: Gen[Any, ModuleName] = moduleName(PathGen.path, NameGen.name)
}

object ModuleNameGen extends ModuleNameGen

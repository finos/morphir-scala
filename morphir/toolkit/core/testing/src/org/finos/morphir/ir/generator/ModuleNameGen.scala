package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.module.QualifiedModuleName
import zio.test.Gen

trait ModuleNameGen {
  final def moduleName[R](namespace: Gen[R, Path], localName: Gen[R, Name]): Gen[R, QualifiedModuleName] =
    for {
      namespace <- namespace
      localName <- localName
    } yield QualifiedModuleName(namespace, localName)

  final val moduleName: Gen[Any, QualifiedModuleName] = moduleName(PathGen.path, NameGen.name)
}

object ModuleNameGen extends ModuleNameGen

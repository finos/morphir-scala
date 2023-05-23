package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.module.QualifiedModuleName
import zio.test.Gen

trait QualifiedModuleNameGen {
  final def qualifiedModuleName[R](namespace: Gen[R, Path], localName: Gen[R, Name]): Gen[R, QualifiedModuleName] =
    for {
      namespace <- namespace
      localName <- localName
    } yield QualifiedModuleName(namespace, localName)

  final val qualifiedModuleName: Gen[Any, QualifiedModuleName] = qualifiedModuleName(PathGen.path, NameGen.name)
}

object QualifiedModuleNameGen extends QualifiedModuleNameGen

package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.Gen

trait QualifiedModuleNameGen {
  final def qualifiedModuleName[R](
      packageName: Gen[R, PackageName],
      moduleName: Gen[R, ModuleName]
  ): Gen[R, QualifiedModuleName] =
    for {
      namespace <- packageName
      localName <- moduleName
    } yield QualifiedModuleName(namespace, localName)

  final val qualifiedModuleName: Gen[Any, QualifiedModuleName] =
    qualifiedModuleName(PackageNameGen.packageName, ModuleNameGen.moduleName)
}

object QualifiedModuleNameGen extends QualifiedModuleNameGen

package org.finos.morphir.ir

import scala.annotation.nowarn

package object module {

  @nowarn
  final case class ModuleName(toPath: Path) extends AnyVal {
    self =>

    def toQualifiedModuleName: QualifiedModuleName = QualifiedModuleName.fromPath(toPath)
    def toQName(localName:Name):QName = QName(toPath, localName)

  }

  object ModuleName {
    def fromString(path: String): ModuleName = ModuleName(Path.fromString(path))
    def fromNames(names:String*):ModuleName = ModuleName(Path.fromIterable(names.map(Name.fromString)))
    def fromPath(path:Path):ModuleName = ModuleName(path)
    implicit def toQualifiedModuleName(moduleName:ModuleName):QualifiedModuleName = moduleName.toQualifiedModuleName
  }
}

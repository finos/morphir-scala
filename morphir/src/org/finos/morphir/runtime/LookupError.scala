package org.finos.morphir.runtime

import org.finos.morphir.naming._

sealed abstract class LookupError(msg: String) extends Exception(msg) {
  def getMsg: String = msg
}
case class MissingPackage(pkgName: PackageName) extends LookupError(s"Package ${pkgName.toString} not found")
case class MissingModule(pkgName: PackageName, modName: ModuleName)
    extends LookupError(s"Package ${pkgName.toString} does not contain module ${modName.toString}")
case class MissingType(pkgName: PackageName, modName: ModuleName, typeName: Name)
    extends LookupError(s"Module ${pkgName.toString}:${modName.toString} has no type named ${typeName.toTitleCase}")
case class MissingDefinition(pkgName: PackageName, modName: ModuleName, defName: Name) extends LookupError(
      s"Module ${pkgName.toString}:${modName.toString} has no definition named ${defName.toCamelCase}"
    )
case class MissingConstructor(pkgName: PackageName, modName: ModuleName, ctorName: Name) extends LookupError(
      s"Module ${pkgName.toString}:${modName.toString} has no constructor named ${ctorName.toTitleCase}"
    )

package org.finos.morphir.universe.ir
import org.finos.morphir.naming.*
final case class FQNameInfo(fqName: FQName, constructorFQName: Option[FQName] = None) {
  def hasConstructorFQName: Boolean = constructorFQName.isDefined
}
object FQNameInfo {
  def apply(packageName: PackageName, modulePath: ModuleName, localName: Name): FQNameInfo =
    FQNameInfo(FQName(packageName, modulePath, localName))

  def fromFQName(fqName: FQName): FQNameInfo = FQNameInfo(fqName)
}

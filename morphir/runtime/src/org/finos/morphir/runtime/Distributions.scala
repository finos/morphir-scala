package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.Module.{Specification => ModSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import zio.Chunk

class Distributions(dists: Map[PackageName, Distribution]) {
  def lookupModuleSpecification(packageName: PackageName, module: ModuleName): Option[ModSpec.Raw] =
    dists.get(packageName) match {
      case Some(Library(_, _, packageDef)) =>
        packageDef.toSpecification.modules.get(module)
      case None => None
    }

    def lookupTypeSpecification(pName: PackageName, module: ModuleName, localName: Name): Option[UTypeSpec] =
      lookupModuleSpecification(pName, module).flatMap(_.lookupTypeSpecification(localName))

    def lookupTypeSpecification(fqn: FQName): Option[UTypeSpec] =
      lookupTypeSpecification(fqn.packagePath, fqn.modulePath, fqn.localName)

  def lookupValueSpecification(
      packageName: PackageName,
      module: ModuleName,
      localName: Name
  ): Option[UValueSpec] =
    lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

  def lookupValueSpecification(
      packageName: PackageName,
      module: ModuleName,
      localName: Name
  ): Option[UValueSpec] =
    lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

  def lookupValueSpecification(fqn: FQName): Option[UValueSpec] =
    lookupValueSpecification(fqn.packagePath, fqn.modulePath, fqn.localName)

  def lookupValueDefinition(
      packageName: PackageName,
      module: ModuleName,
      localName: Name
  ): Option[ValueDefinition[scala.Unit, UType]] =
    lookupModuleDefinition(packageName, module).flatMap(_.lookupValueDefinition(localName))

  def lookupValueDefinition(fqn: FQName): Option[ValueDefinition[scala.Unit, UType]] =
    lookupValueDefinition(fqn.packagePath, fqn.modulePath, fqn.localName)
  def getDists: Map[PackageName, Distribution] = dists
}

object Distributions {
  def apply(dists: Distribution*): Distributions =
    new Distributions(dists.map { case (lib: Library) => lib.packageName -> lib }.toMap)
}

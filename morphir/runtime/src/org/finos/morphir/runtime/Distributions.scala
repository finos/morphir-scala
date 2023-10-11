package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.{
  Value,
  Pattern,
  TypedValue,
  USpecification => UValueSpec,
  TypedDefinition => TypedValueDef
}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.Module.{Specification => ModSpec, Definition => ModDef}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports._
import org.finos.morphir.runtime.MorphirRuntimeError.LookupError
import org.finos.morphir.runtime.MorphirRuntimeError.LookupError.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import zio.Chunk

class Distributions(dists: Map[PackageName, Distribution]) {
  def lookupModuleSpecification(pkgName: PackageName, modName: ModuleName): Either[LookupError, ModSpec.Raw] =
    dists.get(pkgName) match {
      case Some(Library(_, _, packageDef)) =>
        packageDef.toSpecification.modules.get(modName) match {
          case Some(module) => Right(module)
          case None         => Left(new MissingModule(pkgName, modName))
        }
      case None => Left(new MissingPackage(pkgName))
    }

  def lookupModuleDefinition(pkgName: PackageName, modName: ModuleName): Either[LookupError, ModDef[Unit, UType]] =
    dists.get(pkgName) match {
      case Some(Library(_, _, packageDef)) =>
        packageDef.modules.get(modName) match {
          case Some(module) => Right(module.value)
          case None         => Left(new MissingModule(pkgName, modName))
        }
      case None => Left(new MissingPackage(pkgName))
    }

  def lookupTypeSpecification(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, UTypeSpec] =
    lookupModuleSpecification(pkgName, modName).flatMap(_.lookupTypeSpecification(localName) match {
      case Some(tpe) => Right(tpe)
      case None      => Left(new MissingType(pkgName, modName, localName))
    })

  def lookupTypeSpecification(fqn: FQName): Either[LookupError, UTypeSpec] =
    lookupTypeSpecification(fqn.packagePath, fqn.modulePath, fqn.localName)

  def lookupValueSpecification(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, UValueSpec] =
    lookupModuleSpecification(pkgName, modName).flatMap(_.lookupValueSpecification(localName) match {
      case Some(tpe) => Right(tpe)
      case None      => Left(new MissingDefinition(pkgName, modName, localName))
    })

  def lookupValueSpecification(fqn: FQName): Either[LookupError, UValueSpec] =
    lookupValueSpecification(fqn.packagePath, fqn.modulePath, fqn.localName)

  def lookupValueDefinition(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, TypedValueDef] =
    lookupModuleDefinition(pkgName, modName).flatMap(_.lookupValueDefinition(localName) match {
      case Some(tpe) => Right(tpe)
      case None      => Left(new MissingDefinition(pkgName, modName, localName))
    })

  def lookupValueDefinition(fqn: FQName): Either[LookupError, TypedValueDef] =
    lookupValueDefinition(fqn.packagePath, fqn.modulePath, fqn.localName)
  def getDists: Map[PackageName, Distribution] = dists
}

object Distributions {
  def apply(dists: Distribution*): Distributions =
    new Distributions(dists.map { case (lib: Library) => lib.packageName -> lib }.toMap)
}

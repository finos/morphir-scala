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
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification => UTypeSpec, UDefinition => UTypeDef}
import org.finos.morphir.ir.Module.{Specification => ModSpec, Definition => ModDef}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports._
import org.finos.morphir.runtime.MorphirRuntimeError.LookupError
import org.finos.morphir.runtime.MorphirRuntimeError.LookupError.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.*
import zio.Chunk

class Distributions(dists: Map[PackageName, Distribution.Lib]) {
  def lookupModuleSpecification(pkgName: PackageName, modName: ModuleName): Either[LookupError, ModSpec.Raw] =
    dists.get(pkgName) match {
      case Some(Lib(_, packageDef)) =>
        packageDef.toSpecification.modules.get(modName) match {
          case Some(module) => Right(module)
          case None         => Left(new MissingModule(pkgName, modName))
        }
      case None => Left(new MissingPackage(pkgName)
          .withContext(s"Known packages:\n ${dists.keys.mkString("\n  ")}\n"))

    }

  def lookupModuleDefinition(pkgName: PackageName, modName: ModuleName): Either[LookupError, ModDef[Unit, UType]] =
    dists.get(pkgName) match {
      case Some(Lib(_, packageDef)) =>
        packageDef.modules.get(modName) match {
          case Some(module) => Right(module.value)
          case None         => Left(new MissingModule(pkgName, modName))
        }
      case None => Left(new MissingPackage(pkgName)
          .withContext(s"Known packages:\n ${dists.keys.mkString("\n  ")}\n"))
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

  def lookupTypeDefinition(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, UTypeDef] =
    lookupModuleDefinition(pkgName, modName).flatMap(_.lookupTypeDefinition(localName) match {
      case Some(tpe) => Right(tpe)
      case None      => Left(new MissingType(pkgName, modName, localName))
    })

  def lookupTypeDefinition(fqn: FQName): Either[LookupError, UTypeDef] =
    lookupTypeDefinition(fqn.packagePath, fqn.modulePath, fqn.localName)
  def lookupValueSpecification(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, UValueSpec] =
    lookupModuleSpecification(pkgName, modName).flatMap(modSpec =>
      modSpec.lookupValueSpecification(localName) match {
        case Some(tpe) => Right(tpe)
        case None => Left(new MissingDefinition(pkgName, modName, localName)
            .withContext(s"Known definitions from that module:\n ${modSpec.values.keys.mkString("\n  ")}\n"))
      }
    )

  def lookupValueSpecification(fqn: FQName): Either[LookupError, UValueSpec] =
    lookupValueSpecification(fqn.packagePath, fqn.modulePath, fqn.localName)

  def lookupValueDefinition(
      pkgName: PackageName,
      modName: ModuleName,
      localName: Name
  ): Either[LookupError, TypedValueDef] =
    lookupModuleDefinition(pkgName, modName).flatMap(modDef =>
      modDef.lookupValueDefinition(localName) match {
        case Some(tpe) => Right(tpe)
        case None => Left(new MissingDefinition(pkgName, modName, localName)
            .withContext(s"Known definitions from that module:\n ${modDef.values.keys.mkString("\n  ")}\n"))
      }
    )

  def lookupValueDefinition(fqn: FQName): Either[LookupError, TypedValueDef] =
    lookupValueDefinition(fqn.packagePath, fqn.modulePath, fqn.localName)

  def getDists: Map[PackageName, Distribution.Lib] = dists
}

object Distributions {
  def apply(dists: Distribution*): Distributions =
    new Distributions(Distribution.toLibsMapUnsafe(dists: _*))
  def apply(dists: Map[PackageName, Distribution.Lib]): Distributions =
    new Distributions(dists)
}

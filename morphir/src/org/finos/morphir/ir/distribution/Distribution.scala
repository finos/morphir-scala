package org.finos.morphir.ir.distribution
import org.finos.morphir.naming.*
import org.finos.morphir.ir.Module.{Specification => ModSpec}
import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, USpecification => UPackageSpecification}
import org.finos.morphir.ir.Type.{USpecification => UTypeSpec, UDefinition => UTypeDef}
import org.finos.morphir.ir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.ir.Type.Type.Reference
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.{USpecification => UValueSpec, Definition => ValueDefinition}

sealed trait Distribution
object Distribution {
  final case class Lib(
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ) { self =>

    def lookupValueDefinition(qName: QName): Option[ValueDefinition[scala.Unit, UType]] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupValueDefinition(qName.localName))

    def lookupTypeDefinition(qName: QName): Option[UTypeDef] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupTypeDefinition(qName.localName))
  }

  final case class Bundle(
      libraries: Map[PackageName, Lib]
  ) extends Distribution { self =>

    def insert(dist: Distribution): Distribution = dist match {
      case bundle: Bundle   => Bundle(libraries ++ bundle.libraries)
      case library: Library => Bundle(libraries + (library.packageName -> library.toLib))
    }
  }

  final case class Library(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ) extends Distribution { self =>

    def lookupModuleSpecification(packageName: PackageName, module: ModuleName): Option[ModSpec.Raw] =
      self match {
        case Library(`packageName`, _, packageDef) =>
          packageDef.toSpecification.modules.get(module)
        case Library(_, _, _) => None
      }

    def lookupTypeSpecification(pName: PackageName, module: ModuleName, localName: Name): Option[UTypeSpec] =
      lookupModuleSpecification(pName, module).flatMap(_.lookupTypeSpecification(localName))

    def lookupBaseTypeName(fqName: FQName): Option[FQName] =
      lookupModuleSpecification(fqName.packagePath, fqName.getModuleName).flatMap(modSpec =>
        modSpec
          .lookupTypeSpecification(fqName.localName)
          .flatMap(typeSpec =>
            typeSpec match {
              case TypeAliasSpecification(_, Reference(_, aliasFQName, _)) => lookupBaseTypeName(aliasFQName)
              case _                                                       => Some(fqName)
            }
          )
      )

    def lookupValueSpecification(
        packageName: PackageName,
        module: ModuleName,
        localName: Name
    ): Option[UValueSpec] =
      lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

    def lookupPackageSpecification: UPackageSpecification = packageDef.toSpecificationWithPrivate.eraseAttributes

    @inline def lookupPackageName: PackageName = packageName

    def insertDependency(
        dependencyPackageName: PackageName,
        dependencyPackageSpec: UPackageSpecification
    ): Distribution = Library(packageName, dependencies + (dependencyPackageName -> dependencyPackageSpec), packageDef)

    def toLib: Lib = Lib(dependencies, packageDef)
  }

  def toBundle(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ): Bundle = Bundle(Map(packageName -> Lib(dependencies, packageDef)))

  def toBundle(packageName: PackageName, lib: Lib): Bundle = Bundle(Map(packageName -> lib))
  def toBundle(dists: Distribution*): Bundle               = Bundle(toLibsMap(dists: _*))

  def toLibrary(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ): Library = Library(packageName, dependencies, packageDef)

  def toLibrary(packageName: PackageName, lib: Lib): Library = Library(packageName, lib.dependencies, lib.packageDef)
  def toLibraries(dists: Distribution*): List[Library] = toLibsMap(dists: _*)
    .map { case (packageName, lib) => toLibrary(packageName, lib) }
    .toList

  def toLibsMap(dists: Distribution*): Map[PackageName, Lib] =
    dists.flatMap {
      case (library: Library) => List(library.packageName -> library.toLib)
      case (bundle: Bundle)   => bundle.libraries.toList
    }.toMap
}

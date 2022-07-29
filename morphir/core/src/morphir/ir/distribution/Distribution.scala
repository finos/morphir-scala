package morphir.mir.distribution

import morphir.mir.Module.{ModuleName, Specification => ModSpec}
import morphir.mir.PackageModule.{Definition => PackageDefinition, PackageName, USpecification => UPackageSpecification}
import morphir.mir.Type.Specification.{TypeAliasSpecification, USpecification => UTypeSpec}
import morphir.mir.Type.Type.Reference
import morphir.mir.Type.UType
import morphir.mir.Value.{USpecification => UValueSpec, ValueDefinition}
import morphir.mir.{FQName, Name, QName}

sealed trait Distribution
object Distribution {
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

    def lookupValueDefinition(qName: QName): Option[ValueDefinition[Any, UType]] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupValueDefinition(qName.localName))

    def lookupPackageSpecification: UPackageSpecification = packageDef.toSpecificationWithPrivate.eraseAttributes

    @inline def lookupPackageName: PackageName = packageName

    def insertDependency(
        dependencyPackageName: PackageName,
        dependencyPackageSpec: UPackageSpecification
    ): Distribution = Library(packageName, dependencies + (dependencyPackageName -> dependencyPackageSpec), packageDef)
  }
}

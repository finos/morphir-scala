package org.finos.morphir.ir.distribution
import org.finos.morphir.naming.*
import org.finos.morphir.ir.Module.{Specification => ModSpec}
import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, USpecification => UPackageSpecification}
import org.finos.morphir.ir.Type.{USpecification => UTypeSpec, UDefinition => UTypeDef}
import org.finos.morphir.ir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.ir.Type.Type.Reference
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.{USpecification => UValueSpec, Definition => ValueDefinition}
import scala.collection.immutable.MultiDict
import scala.annotation.tailrec
import org.finos.morphir.ir.distribution.Distribution.RepeatedPackages.Allowed
import org.finos.morphir.ir.distribution.Distribution.RepeatedPackages.NotAllowed

sealed trait Distribution { self =>
  import Distribution._

  /// Get all distributions contained in this distribution.
  /// Note: This will expand bundles but not dependencies.
  def allDistributions: List[Distribution] = {
    @tailrec
    def loop(pending: List[Distribution], acc: List[Distribution]): List[Distribution] = pending match {
      case Nil                              => acc
      case (lib @ Library(_, _, _)) :: rest => loop(rest, lib :: acc)
      case (bundle @ Bundle(_)) :: rest     => loop(bundle.toLibraries ++ rest, acc)
    }
    loop(self :: Nil, List.empty)
  }
}
object Distribution {
  final case class Lib(
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ) { self =>

    def lookupValueDefinition(qName: QName): Option[ValueDefinition[scala.Unit, UType]] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupValueDefinition(qName.localName))

    def lookupTypeDefinition(qName: QName): Option[UTypeDef] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupTypeDefinition(qName.localName))

    private[Distribution] def toLibrary(packageName: PackageName): Library =
      Library(packageName, self.dependencies, self.packageDef)
  }

  final case class Bundle(
      libraries: Map[PackageName, Lib]
  ) extends Distribution { self =>

    def insert(dist: Distribution): Distribution = dist match {
      case bundle: Bundle   => Bundle(libraries ++ bundle.libraries)
      case library: Library => Bundle(libraries + (library.packageName -> library.toLib))
    }

    private[Distribution] def toLibraries: List[Library] = self.libraries.map {
      case (packageName, lib) => lib.toLibrary(packageName)
    }.toList
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

  def toBundleUnsafe(distributions: Distribution*): Bundle = toBundleUnsafe(BundleSettings.default, distributions: _*)
  def toBundleUnsafe(settings: BundleSettings, distributions: Distribution*): Bundle = settings.RepeatedPackages match {
    case RepeatedPackages.Allowed =>
      val map = distributions.flatMap {
        case (library: Library) => List(library.packageName -> library.toLib)
        case (bundle: Bundle)   => bundle.libraries.toList
      }.toMap
      Bundle(map)
    case RepeatedPackages.NotAllowed =>
      val lookup           = toLookup(distributions: _*)
      val repeatedPackages = lookup.repeatedPackages
      if (repeatedPackages.nonEmpty) {
        throw new BundlingError.MultiplePackagesWithSameNameDetected(repeatedPackages)
      }
      Bundle(lookup.toMultiDict.toMap)
  }

  def toLibrary(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ): Library = Library(packageName, dependencies, packageDef)

  def toLibrary(packageName: PackageName, lib: Lib): Library = Library(packageName, lib.dependencies, lib.packageDef)

  def toLibraries(distributions: Distribution*): List[Library] = {
    @tailrec
    def loop(pending: List[Distribution], acc: List[Library]): List[Library] = pending match {
      case Nil                              => acc
      case (lib @ Library(_, _, _)) :: rest => loop(rest, lib :: acc)
      case Bundle(libraries) :: rest =>
        val newLibs = libraries.map { case (packageName, lib) => lib.toLibrary(packageName) }.toList
        loop(rest, newLibs ++ acc)
    }
    loop(distributions.toList, List.empty)
  }

  def toLookup(distributions: Distribution*): LibLookup = LibLookup.fromDistributions(distributions)

  def toLibsMapUnsafe(distributions: Distribution*): Map[PackageName, Lib] =
    toLibsMapUnsafe(RepeatedPackages.NotAllowed, distributions: _*)

  def toLibsMapUnsafe(repeatedPackages: RepeatedPackages, distributions: Distribution*): Map[PackageName, Lib] = {
    val lookup = toLookup(distributions: _*)
    repeatedPackages match {
      case Allowed => lookup.toMultiDict.toMap
      case NotAllowed =>
        val lookup           = toLookup(distributions: _*)
        val repeatedPackages = lookup.repeatedPackages
        if (repeatedPackages.nonEmpty) {
          throw new BundlingError.MultiplePackagesWithSameNameDetected(repeatedPackages)
        }
        lookup.toMultiDict.toMap
    }
  }

  final case class LibLookup(toMultiDict: MultiDict[PackageName, Lib]) extends AnyVal { self =>
    def packageNames: scala.collection.Set[PackageName] = toMultiDict.keySet
    def repeatedPackages: Set[PackageName] =
      toMultiDict.sets.collect { case (packageName, libs) if libs.size > 1 => packageName }.toSet
  }

  object LibLookup {
    def apply(distributions: Distribution*): LibLookup = fromDistributions(distributions)

    def fromDistributions(distributions: Seq[Distribution]): LibLookup = {
      def loop(pending: List[Distribution], acc: MultiDict[PackageName, Lib]): LibLookup = pending match {
        case Nil => LibLookup(acc)
        case Library(packageName, dependencies, packageDef) :: rest =>
          loop(rest, acc.add(packageName, Lib(dependencies, packageDef)))
        case (bundle @ Bundle(_)) :: rest => loop(bundle.toLibraries ++ rest, acc)
      }
      loop(distributions.toList, MultiDict.empty)
    }
  }

  trait DistributionError

  sealed abstract class BundlingError(message: String) extends Exception(message) with DistributionError with Product
      with Serializable
  object BundlingError {

    def failWithMultiplePackagesWithSameNameDetected(
        packageName: PackageName,
        others: PackageName*
    ): MultiplePackagesWithSameNameDetected = others match {
      case Nil => MultiplePackagesWithSameNameDetected(Set(packageName))
      case _   => MultiplePackagesWithSameNameDetected(Set(packageName) ++ Set.from(others))
    }

    final case class MultiplePackagesWithSameNameDetected private[Distribution] (packages: Set[PackageName])
        extends BundlingError(
          s"Multiple packages with the same name detected. Repeated packages are: ${packages.mkString(", ")}"
        ) { self =>
      def +(other: PackageName): MultiplePackagesWithSameNameDetected =
        MultiplePackagesWithSameNameDetected(self.packages + other)
      def +:(other: PackageName): MultiplePackagesWithSameNameDetected =
        MultiplePackagesWithSameNameDetected(self.packages + other)

    }
  }

  final case class BundleSettings(RepeatedPackages: RepeatedPackages)
  object BundleSettings {
    val default: BundleSettings = BundleSettings(RepeatedPackages.NotAllowed)
  }

  sealed abstract class RepeatedPackages extends Product with Serializable
  object RepeatedPackages {
    case object Allowed    extends RepeatedPackages
    case object NotAllowed extends RepeatedPackages
  }

}

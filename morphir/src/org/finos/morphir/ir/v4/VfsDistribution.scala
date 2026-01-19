package org.finos.morphir
package ir
package v4

import org.finos.morphir.naming._
import zio.Chunk

// Documentation types
final case class Documentation(lines: Chunk[String])
final case class Documented[+A](doc: Option[Documentation], value: A)

// Distribution types
enum Distribution {
  case Library(library: LibraryDistribution)
  case Specs(specs: SpecsDistribution)
  case Application(application: ApplicationDistribution)
}

final case class LibraryDistribution(
    packageInfo: PackageInfo,
    definition: PackageDefinition,
    dependencies: Map[PackageName, PackageSpecification]
)

final case class SpecsDistribution(
    packageInfo: PackageInfo,
    specification: PackageSpecification,
    dependencies: Map[PackageName, PackageSpecification]
)

final case class ApplicationDistribution(
    packageInfo: PackageInfo,
    definition: PackageDefinition,
    dependencies: Map[PackageName, PackageDefinition],
    entryPoints: Map[Name, EntryPoint]
)

final case class PackageInfo(
    name: PackageName,
    version: String
) // Using String for version for now, could be SemanticVersion

// Package types
final case class PackageDefinition(
    modules: Map[ModuleName, AccessControlled[ModuleDefinition]]
)

final case class PackageSpecification(
    modules: Map[ModuleName, ModuleSpecification]
)

// Module types
final case class ModuleDefinition(
    types: Map[Name, AccessControlled[Documented[TypeDefinition]]],
    values: Map[Name, AccessControlled[Documented[ValueDefinition]]]
)

final case class ModuleSpecification(
    types: Map[Name, Documented[TypeSpecification]],
    values: Map[Name, Documented[ValueSpecification]]
)

// Entry Point types
final case class EntryPoint(target: FQName, kind: EntryPointKind, doc: Option[Documentation])

enum EntryPointKind {
  case Main
  case Command
  case Handler
  case Job
  case Policy
}

// VfsManifest maps to format.json
final case class VfsManifest(
    formatVersion: String,
    layout: DistributionMode,
    packageName: PackageName,
    created: String
)

enum DistributionMode {
  case ClassicMode
  case VfsMode
}

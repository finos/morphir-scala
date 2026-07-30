package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk
import kyo.Schema

// Documentation types
final case class Documentation(lines: Chunk[String]) derives Schema
final case class Documented[+A](doc: Option[Documentation], value: A) derives Schema

// Distribution types
enum Distribution derives Schema {
  case Library(library: LibraryDistribution)
  case Specs(specs: SpecsDistribution)
  case Application(application: ApplicationDistribution)
}

final case class LibraryDistribution(
    packageInfo: PackageInfo,
    definition: PackageDefinition,
    dependencies: Map[PackageName, PackageSpecification]
) derives Schema

final case class SpecsDistribution(
    packageInfo: PackageInfo,
    specification: PackageSpecification,
    dependencies: Map[PackageName, PackageSpecification]
) derives Schema

final case class ApplicationDistribution(
    packageInfo: PackageInfo,
    definition: PackageDefinition,
    dependencies: Map[PackageName, PackageDefinition],
    entryPoints: Map[Name, EntryPoint]
) derives Schema

final case class PackageInfo(
    name: PackageName,
    version: String
) derives Schema // Using String for version for now, could be SemanticVersion

// Package types
final case class PackageDefinition(
    modules: Map[ModuleName, AccessControlled[ModuleDefinition]]
) derives Schema

final case class PackageSpecification(
    modules: Map[ModuleName, ModuleSpecification]
) derives Schema

// Module types
final case class ModuleDefinition(
    types: Map[Name, AccessControlled[Documented[TypeDefinition]]],
    values: Map[Name, AccessControlled[Documented[ValueDefinition]]]
) derives Schema

final case class ModuleSpecification(
    types: Map[Name, Documented[TypeSpecification]],
    values: Map[Name, Documented[ValueSpecification]]
) derives Schema

// Entry Point types
final case class EntryPoint(target: FQName, kind: EntryPointKind, doc: Option[Documentation]) derives Schema

enum EntryPointKind derives Schema {
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
) derives Schema

enum DistributionMode derives Schema {
  case ClassicMode
  case VfsMode
}

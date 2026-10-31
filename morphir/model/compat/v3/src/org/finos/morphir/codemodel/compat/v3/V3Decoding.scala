package org.finos.morphir.codemodel.compat.v3

import kyo.Chunk
import kyo.Result
import kyo.Structure
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName
import org.finos.morphir.naming.ModuleName
import org.finos.morphir.naming.Name
import org.finos.morphir.naming.PackageName

import scala.math.Ordering.Implicits.seqOrdering

/**
 * The node decoders behind [[V3Reader]]. Each takes the parsed JSON node and its JSON path, for error locations.
 *
 * Names, literals, types, patterns and values live in [[V3NamingDecoding]], [[V3TypeDecoding]] and [[V3ValueDecoding]];
 * this object adds modules, packages and distributions, and is the one entry point the reader uses.
 */
private[v3] object V3Decoding:
  import V3JsonShape.*
  import V3Reader.Read

  /** The package version a decoded library gets: v3 records none, and the v3 lowering leaves it empty too. */
  val unknownPackageVersion: String = ""

  def irFile(value: Value, path: String): Read[Chunk[cm.Distribution]] =
    value match
      case Structure.Value.Record(fields) =>
        val byName = fields.toMap
        byName.get("formatVersion") match
          case None                             => wrongShape(member(path, "formatVersion"), "a formatVersion member")
          case Some(Structure.Value.Integer(3)) =>
            byName.get("distribution") match
              case Some(distributionValue) => distribution(distributionValue, member(path, "distribution"))
              case None                    => wrongShape(member(path, "distribution"), "a distribution member")
          case Some(other) => Result.fail(V3ReadError.UnsupportedFormatVersion(render(other)))
      case _ => wrongShape(path, "an object with formatVersion and distribution")

  /** `["Library", packageName, dependencies, packageDefinition]`, or `["Bundle", [[packageName, library], …]]`. */
  def distribution(value: Value, path: String): Read[Chunk[cm.Distribution]] =
    tagged(value, path) { (tag, args) =>
      def arg(index: Int): String = at(path, index + 1)
      tag match
        case "Library" =>
          arguments(tag, args, 3, path) {
            for
              libraryName <- V3NamingDecoding.packageName(args(0), arg(0))
              deps        <- dependencies(args(1), arg(1))
              definition  <- packageDefinition(args(2), arg(2))
            yield Chunk(library(libraryName, deps, definition))
          }
        case "Bundle" =>
          arguments(tag, args, 1, path) {
            list(args(0), arg(0))((entry, entryPath) =>
              pair(entry, entryPath)(V3NamingDecoding.packageName, bundledLibrary)
            ).map { entries =>
              Chunk.from(
                entries.toMap.toSeq
                  .sortBy(_._1.toPath.segments.toList.map(_.toList))
                  .map { case (packageName, (dependencies, definition)) =>
                    library(packageName, dependencies, definition)
                  }
              )
            }
          }
        case other => unknownTag(path, other)
    }

  private def library(
      packageName: PackageName,
      dependencies: Map[PackageName, cm.PackageSpecification],
      definition: cm.PackageDefinition
  ): cm.Distribution =
    cm.Distribution.Library(
      cm.LibraryDistribution(cm.PackageInfo(packageName, unknownPackageVersion), definition, dependencies)
    )

  /** A bundled library: `{"dependencies": …, "packageDef": …}`. */
  private def bundledLibrary(
      value: Value,
      path: String
  ): Read[(Map[PackageName, cm.PackageSpecification], cm.PackageDefinition)] =
    obj(value, path, "an object with dependencies and packageDef") { members =>
      for
        deps       <- members.read("dependencies")(dependencies)
        definition <- members.read("packageDef")(packageDefinition)
      yield (deps, definition)
    }

  private def dependencies(value: Value, path: String): Read[Map[PackageName, cm.PackageSpecification]] =
    entries(value, path)(V3NamingDecoding.packageName, packageSpecification)

  /** `{"modules": [[moduleName, {"access": …, "value": moduleDefinition}], …]}` */
  def packageDefinition(value: Value, path: String): Read[cm.PackageDefinition] =
    obj(value, path, "an object with modules") { members =>
      members
        .read("modules")(entries(_, _)(V3NamingDecoding.moduleName, accessControlled(_, _)(moduleDefinition)))
        .map(cm.PackageDefinition(_))
    }

  /** `{"modules": [[moduleName, moduleSpecification], …]}` */
  def packageSpecification(value: Value, path: String): Read[cm.PackageSpecification] =
    obj(value, path, "an object with modules") { members =>
      members
        .read("modules")(entries(_, _)(V3NamingDecoding.moduleName, moduleSpecification))
        .map(cm.PackageSpecification(_))
    }

  /**
   * `{"types": [[name, {"access": …, "value": {"doc": …, "value": typeDefinition}}], …], "values": […]}`. A value's
   * entry access is also the access of its definition body, as in the v3 lowering. Other members, such as the module
   * `doc` morphir-elm writes, are ignored.
   */
  def moduleDefinition(value: Value, path: String): Read[cm.ModuleDefinition] =
    obj(value, path, "an object with types and values") { members =>
      for
        types <- members.read("types")(
          entries(_, _)(V3NamingDecoding.name, accessControlled(_, _)(documented(_, _)(typeDefinition)))
        )
        values <- members.read("values")(entries(_, _)(V3NamingDecoding.name, valueEntry))
      yield cm.ModuleDefinition(types, values)
    }

  private def valueEntry(value: Value, path: String): Read[cm.AccessControlled[cm.Documented[cm.ValueDefinition]]] =
    obj(value, path, "an object with access and value") { members =>
      for
        level      <- members.read("access")(access)
        definition <- members.read("value")(documented(_, _)(V3ValueDecoding.valueDefinition(level)))
      yield cm.AccessControlled(level, definition)
    }

  /** `{"types": [[name, {"doc": …, "value": typeSpecification}], …], "values": […]}` */
  def moduleSpecification(value: Value, path: String): Read[cm.ModuleSpecification] =
    obj(value, path, "an object with types and values") { members =>
      for
        types  <- members.read("types")(entries(_, _)(V3NamingDecoding.name, documented(_, _)(typeSpecification)))
        values <- members.read("values")(entries(_, _)(V3NamingDecoding.name, documented(_, _)(valueSpecification)))
      yield cm.ModuleSpecification(types, values)
    }

  /** A v3 map, written as an array of `[key, value]` pairs; a repeated key keeps its last entry. */
  private def entries[K, V](value: Value, path: String)(
      key: (Value, String) => Read[K],
      entryValue: (Value, String) => Read[V]
  ): Read[Map[K, V]] =
    list(value, path)((entry, entryPath) => pair(entry, entryPath)(key, entryValue)).map(_.toMap)

  def name(value: Value, path: String): Read[Name]                          = V3NamingDecoding.name(value, path)
  def path(value: Value, path: String): Read[org.finos.morphir.naming.Path] = V3NamingDecoding.namingPath(value, path)
  def fqName(value: Value, path: String): Read[FQName]                      = V3NamingDecoding.fqName(value, path)
  def literal(value: Value, path: String): Read[cm.Literal]                 = V3NamingDecoding.literal(value, path)
  def tpe(value: Value, path: String): Read[cm.Type]                        = V3TypeDecoding.tpe(value, path)
  def pattern(value: Value, path: String): Read[cm.Pattern]                 = V3ValueDecoding.pattern(value, path)
  def value(value: Value, path: String): Read[cm.Expr]                      = V3ValueDecoding.expr(value, path)
  def typeDefinition(value: Value, path: String): Read[cm.TypeDefinition]   = V3TypeDecoding.typeDefinition(value, path)
  def typeSpecification(value: Value, path: String): Read[cm.TypeSpecification] =
    V3TypeDecoding.typeSpecification(value, path)

  /** A bare v3 value definition carries no access of its own; it decodes as public. */
  def valueDefinition(value: Value, path: String): Read[cm.ValueDefinition] =
    V3ValueDecoding.valueDefinition(cm.Access.Public)(value, path)

  def valueSpecification(value: Value, path: String): Read[cm.ValueSpecification] =
    V3ValueDecoding.valueSpecification(value, path)

  private def render(value: Value): String = value match
    case Structure.Value.Integer(n) => n.toString
    case Structure.Value.Str(s)     => s"\"$s\""
    case other                      => other.toString
end V3Decoding

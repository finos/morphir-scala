package org.finos.morphir.codemodel.compat.v3

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.Present
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

  /** The package version a decoded library gets: v3 records none, so it is left empty. */
  val unknownPackageVersion: String = ""

  /**
   * A whole document, `{"formatVersion": …, "distribution": …}`, in either member order. The `formatVersion` is read
   * and checked before anything else, so an unsupported document fails with a format-version fault rather than with
   * whatever its distribution would have caused.
   */
  def irDocument(value: Value, path: String): Read[V3Document] =
    value match
      case Structure.Value.Record(fields) =>
        V3FormatVersion.ofRoot(fields).flatMap { release =>
          fields.find(_._1 == "distribution") match
            case Some((_, distributionValue)) =>
              distribution(distributionValue, member(path, "distribution"), Present(release))
                .map(V3Document(release, _))
            case None => wrongShape(member(path, "distribution"), "a distribution member")
        }
      case _ => wrongShape(path, "an object with formatVersion and distribution")

  /**
   * `["Library", packageName, dependencies, packageDefinition]`, `["Specs", packageName, dependencies,
   * packageSpecification]`, or `["Bundle", [[packageName, library], …]]`. With the document's `release` known, a
   * `Specs` distribution must declare 3.1.0 or later.
   */
  def distribution(value: Value, path: String, release: Maybe[V3Release]): Read[Chunk[cm.Distribution]] =
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
        case "Specs" =>
          release.filter(!_.hasSpecs) match
            case Present(early) => Result.fail(V3ReadError.SpecsBefore31(early.canonicalJson))
            case _              =>
              arguments(tag, args, 3, path) {
                for
                  specsName     <- V3NamingDecoding.packageName(args(0), arg(0))
                  deps          <- dependencies(args(1), arg(1))
                  specification <- packageSpecification(args(2), arg(2))
                yield Chunk(
                  cm.Distribution.Specs(
                    cm.SpecsDistribution(cm.PackageInfo(specsName, unknownPackageVersion), specification, deps)
                  )
                )
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
        .read("modules")(entries(
          _,
          _
        )(V3NamingDecoding.moduleName, accessControlled(_, _)(moduleDefinition(_, _, V3AttributeRule.TypeRequired))))
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
   * `{"types": [[name, {"access": …, "value": {"doc": …, "value": typeDefinition}}], …], "values": […], "doc": …}`. A
   * value's entry access is also the access of its definition body. Other members are ignored.
   */
  def moduleDefinition(value: Value, path: String, rule: V3AttributeRule): Read[cm.ModuleDefinition] =
    given V3AttributeRule = rule
    obj(value, path, "an object with types and values") { members =>
      for
        types <- members.read("types")(
          entries(_, _)(V3NamingDecoding.name, accessControlled(_, _)(documented(_, _)(typeDefinition)))
        )
        values <- members.read("values")(entries(_, _)(V3NamingDecoding.name, valueEntry))
        doc    <- moduleDoc(members)
      yield cm.ModuleDefinition(types, values, doc)
    }

  /**
   * A module's optional `doc`: a string, or `null` or absent for none, as morphir-elm writes it. The string is kept
   * line by line; unlike an entry's doc, an empty string stays documentation, so that `""` and `null` read back apart.
   */
  private def moduleDoc(members: Members): Read[Maybe[cm.Documentation]] =
    members.readOptional("doc")(string).map { text =>
      Maybe.fromOption(text).map(doc => cm.Documentation(Chunk.from(doc.linesIterator.toSeq)))
    }

  private def valueEntry(value: Value, path: String)(using
      V3AttributeRule
  ): Read[cm.AccessControlled[cm.Documented[cm.ValueDefinition]]] =
    obj(value, path, "an object with access and value") { members =>
      for
        level      <- members.read("access")(access)
        definition <- members.read("value")(documented(_, _)(V3ValueDecoding.valueDefinition(level)))
      yield cm.AccessControlled(level, definition)
    }

  /** `{"types": [[name, {"doc": …, "value": typeSpecification}], …], "values": […], "doc": …}` */
  def moduleSpecification(value: Value, path: String): Read[cm.ModuleSpecification] =
    obj(value, path, "an object with types and values") { members =>
      for
        types  <- members.read("types")(entries(_, _)(V3NamingDecoding.name, documented(_, _)(typeSpecification)))
        values <- members.read("values")(entries(_, _)(V3NamingDecoding.name, documented(_, _)(valueSpecification)))
        doc    <- moduleDoc(members)
      yield cm.ModuleSpecification(types, values, doc)
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
  def pattern(value: Value, path: String): Read[cm.Pattern]                 =
    V3ValueDecoding.pattern(value, path)(using V3AttributeRule.TypeOptional)
  def value(value: Value, path: String): Read[cm.Expr] =
    V3ValueDecoding.expr(value, path)(using V3AttributeRule.TypeOptional)
  def typeDefinition(value: Value, path: String): Read[cm.TypeDefinition] = V3TypeDecoding.typeDefinition(value, path)
  def typeSpecification(value: Value, path: String): Read[cm.TypeSpecification] =
    V3TypeDecoding.typeSpecification(value, path)

  /** A bare v3 value definition carries no access of its own; it decodes as public. */
  def valueDefinition(value: Value, path: String): Read[cm.ValueDefinition] =
    V3ValueDecoding.valueDefinition(cm.Access.Public)(value, path)(using V3AttributeRule.TypeOptional)

  def valueSpecification(value: Value, path: String): Read[cm.ValueSpecification] =
    V3ValueDecoding.valueSpecification(value, path)

  def formatVersion(value: Value, path: String): Read[V3Release] = V3FormatVersion.release(value)
end V3Decoding

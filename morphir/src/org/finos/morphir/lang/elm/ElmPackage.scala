package org.finos.morphir.lang.elm

import semver.Version
import zio.Chunk
import zio.prelude._

final case class ElmPackage(
    name: ElmPackageName,
    summary: String,
    license: String,
    version: Version,
    exposedModules: Exposed,
    elmVersion: Range,
    dependencies: Map[ElmPackageName, Range],
    testDependencies: Map[ElmPackageName, Range]
)

object ElmPackage {
  def apply(name: ElmPackageName, summary: String, license: String): ElmPackage = {
    val dependencies = Map[ElmPackageName, Range]()
    ElmPackage(
      name = name,
      summary = summary,
      license = license,
      version = Version(1, 0, 0),
      exposedModules = Exposed.Plain.empty,
      elmVersion = Range(Version(0, 19, 0), Version(0, 20, 0), false),
      dependencies = dependencies,
      testDependencies = Map.empty
    )
  }
}

final case class Range(lower: Version, upper: Version, upperInclusive: Boolean) { self => }

sealed trait Exposed extends Product with Serializable
object Exposed {
  final case class Plain(items: List[String]) extends Exposed
  object Plain {
    val empty: Plain = Plain(List.empty)
  }
  final case class Structured(items: Map[String, List[String]]) extends Exposed
  object Structured {
    val empty: Structured = Structured(Map.empty)
  }
}

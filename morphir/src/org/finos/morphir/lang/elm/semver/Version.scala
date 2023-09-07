package org.finos.morphir.lang.elm.semver

import enumeratum.values._
import zio.Chunk
import zio.prelude._

/// A semantic version number, as used by the Elm tooling.
/// NOTE: unlike typical semver, this does not support pre-release or build metadata.
final case class Version(major: Long, minor: Long, patch: Long) { self =>
  override def toString: String = s"$major.$minor.$patch"
}

object Version {
  def fromString(version: String): Validation[String, Version] = {
    val parts = version.split('.').zipWithIndex.collect { case (part, idx) =>
      part.toLongOption match {
        case None        => Validation.fail(s"Invalid version part($idx): $part")
        case Some(value) => Validation.succeed(value)
      }
    }

    Validation.validateAll(Chunk.fromIterable(parts)).flatMap {
      case Chunk(major, minor, patch) =>
        Validation.succeed(Version(major, minor, patch))
      case Chunk(major, minor) =>
        Validation.succeed(Version(major, minor, 0))
      case Chunk(major) =>
        Validation.succeed(Version(major, 0, 0))
      case _ => Validation.fail(s"Invalid version: $version")
    }
  }
}

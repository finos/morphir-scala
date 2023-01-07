package org.finos.morphir.core.types

object Versioning:
  opaque type MorphirVersion = String

  object MorphirVersion:
    def apply(version: String): MorphirVersion = version

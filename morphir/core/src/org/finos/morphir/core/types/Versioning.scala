package org.finos.morphir.core.types
import monix.newtypes.*
object Versioning {
  type MorphirVersion = String

  object MorphirVersion extends Newtype[String] {
    def apply(version: String): MorphirVersion = version
  }
}

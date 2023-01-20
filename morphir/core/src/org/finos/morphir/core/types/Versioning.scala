package org.finos
package morphir
package core.types

import morphir.prelude.*
object Versioning {
  type MorphirVersion = MorphirVersion.Type

  object MorphirVersion extends Newtype[String] {}
}

package org.finos
package morphir
package core.types

import morphir.foundations.*
object Versioning {
  type MorphirVersion = MorphirVersion.Type

  object MorphirVersion extends Newtype[String] {}
}

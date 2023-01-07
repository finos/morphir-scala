package org.finos.morphir
package core
package internal

private[core] object InternalTypes:
  opaque type MorphirVersion = String
  object MorphirVersion:
    def apply(version: String): MorphirVersion = version

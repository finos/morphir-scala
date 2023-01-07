package org.finos
package morphir.concepts

import morphir.core.types.Versioning.MorphirVersion

enum Distro:
  case Library
  case Bundle(morphirVersion: MorphirVersion)

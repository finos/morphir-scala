package org.finos.morphir.ir.internal
import org.finos.morphir.ir.internal.naming.PackageName

enum Distribution:
  case Library(packageName: PackageName)

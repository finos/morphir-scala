package org.finos
package morphir.ir

import morphir.ir.Package.PackageName
object Distribution {

  sealed trait Distribution
  object Distribution {
    final case class Library(packageName: PackageName)
  }
}

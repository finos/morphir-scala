package org.finos
package morphir
package ir

import ir.Package.{PackageName, Definition => PkgDef, Specification => PkgSpec}
import zio.prelude.*

object Distribution {

  sealed trait Distribution
  object Distribution {
    final case class Library(packageName: PackageName) extends Distribution

    type Dependencies = Dependencies.Type
    object Dependencies extends Subtype[Map[PackageName, PkgSpec[Unit]]]
  }
}

package org.finos.morphir
package lang.scaladsl

import org.finos.morphir.ir.Package.PackageName
import org.finos.morphir.ir.Module.ModuleName
sealed trait Syntax
object Syntax:
  sealed trait Definition extends Syntax
  sealed trait Distro     extends Syntax

  case class BundleInfo(name: ir.Package.PackageName)
  case class Bundle(info: BundleInfo, modules: List[Module])                              extends Distro
  case class Module(name: ModuleName, body: Map[ir.Name.Name, ir.Module.ModuleDefOrSpec]) extends Definition
end Syntax

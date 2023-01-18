package org.finos
package morphir
package lang.scaladsl

import ir.Module.{ModuleName, ModuleDefOrSpec}
import ir.Name.Name
import ir.Package.PackageName
sealed trait Syntax
object Syntax:
  sealed trait Definition extends Syntax
  sealed trait Distro     extends Syntax

  case class BundleInfo(name: PackageName)
  case class Bundle(info: BundleInfo, modules: List[Module])                      extends Distro
  case class Module(name: ir.Module.ModuleName, body: Map[Name, ModuleDefOrSpec]) extends Definition
end Syntax

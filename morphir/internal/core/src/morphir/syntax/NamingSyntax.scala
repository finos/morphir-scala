package org.finos.morphir.syntax

import org.finos.morphir.mir.PackageModule.PackageName
import org.finos.morphir.mir.{FQName, Name}

trait NamingSyntax {
  def fqn(packageName: String, module: String, localName: String): FQName = FQName.fqn(packageName, module, localName)
  def name(name: String): Name                                            = Name.fromString(name)
  def pkg(name: String): PackageName                                      = PackageName.fromString(name)
}

object NamingSyntax extends NamingSyntax

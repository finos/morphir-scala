package org.finos.morphir
import scala.annotation.tailrec
import zio.=!=
import zio.prelude.*

object naming
    extends FQNameExports
    with ModuleNameExports
    with NameExports
    with NamespaceExports
    with NodeIDExports
    with PathExports
    with PackageNameExports
    with QualifiedModuleNameExports
    with QNameExports {}

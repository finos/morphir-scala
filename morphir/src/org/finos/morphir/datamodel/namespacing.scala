package org.finos.morphir.datamodel
import org.finos.morphir.naming
import org.finos.morphir.naming.*
import org.finos.morphir.core.capabilities.*
import zio.*
import zio.prelude.*
object namespacing {

  // TODO Remove LocalName
  val ns   = Namespace.ns
  val root = PackageName.root
}

package org.finos.morphir.datamodel
import org.finos.morphir.naming
import org.finos.morphir.naming.*
import org.finos.morphir.core.capabilities.*
import zio.*
import zio.prelude.*
object namespacing {

  def localName(name: String): LocalName = LocalName(name)

  val ns = Namespace.ns

  // TODO Remove LocalName
  type LocalName = LocalName.Type
  object LocalName extends Subtype[String] {
    implicit final class LocalNameOps(val self: LocalName) extends AnyVal {
      def value: String = unwrap(self)
      def toName: Name  = Name.fromString(unwrap(self))
      def toQualified(pack: PackageName, namespace: Namespace): FQName =
        FQName(pack, namespace.toModuleName, self.toName)
      def /:(partialName: QualifiedModuleName): FQName =
        FQName.fromLocalName(self.value)(partialName)
    }
  }

  val root = PackageName.root
}

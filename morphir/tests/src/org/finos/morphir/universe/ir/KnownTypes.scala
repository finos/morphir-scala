package org.finos.morphir.universe.ir
import org.finos.morphir.naming._
import org.finos.morphir.util.attribs._

object KnownTypes {
  val int                                 = Type.Reference(Attributes.empty, pkg"Morphir.SDK" % "Basics" % "Int", Nil)
  val bool                                = Type.Reference(Attributes.empty, pkg"Morphir.SDK" % "Basics" % "Bool", Nil)
  val float                               = Type.Reference(Attributes.empty, pkg"Morphir.SDK" % "Basics" % "Float", Nil)
  def list(elementType: Type[Attributes]) =
    Type.Reference(Attributes.empty, pkg"Morphir.SDK" % "List" % "List", elementType :: Nil)
  val string = Type.Reference(Attributes.empty, pkg"Morphir.SDK" % "Basics" % "String", Nil)
}

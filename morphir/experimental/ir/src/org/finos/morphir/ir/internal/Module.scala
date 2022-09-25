package org.finos.morphir.ir.internal

import org.finos.morphir.ir.internal.naming.FQName

final case class Module[+TAttr, +VAttr](
    name: FQName,
    types: List[Type[TAttr]],
    values: List[Value[TAttr, VAttr]]
) {

  def fold[Z](folder: Module.Folder[TAttr, VAttr, Z]): Z =
    folder.moduleCase(
      name,
      types.map(_.fold(folder.typeCase)),
      values.map(_.fold(folder.valueCase))
    )
}

object Module:
  trait Folder[-TAttr, -VAttr, Z] {
    def moduleCase(name: FQName, types: List[Z], values: List[Z]): Z
    def typeCase: Type.Folder[TAttr, Z]
    def valueCase: Value.Folder[TAttr, VAttr, Z]
  }

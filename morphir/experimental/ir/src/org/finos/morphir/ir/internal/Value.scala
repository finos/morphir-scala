package org.finos.morphir.ir.internal

enum Value[+TAttr, +VAttr]:
  case Unit(attributes: VAttr)

  def fold[Attr1 >: TAttr, Attr2 >: VAttr, Z](folder: Value.Folder[Attr1, Attr2, Z]): Z =
    this match
      case Unit(attributes) =>
        folder.unitCase(attributes)

object Value:
  trait Folder[-TAttr, -VAttr, Z] {
    def unitCase(attributes: VAttr): Z
  }

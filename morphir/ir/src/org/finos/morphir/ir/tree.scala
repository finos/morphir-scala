package org.finos.morphir.ir

object tree:
  enum Type[+Attr]:
    case Unit(attributes: Attr)

  enum Value[+TAttr, +VAttr]:
    case Unit(attributes: VAttr)

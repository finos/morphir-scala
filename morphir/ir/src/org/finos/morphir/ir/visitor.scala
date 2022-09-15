package org.finos.morphir.ir

object visitor:
  abstract class MorphirVisitor[+TAttr, +VAttr, +A]:
    def visitString(value: String): A
    def visitType(): TypeVisitor[TAttr, A]

  abstract class TypeVisitor[+TAttr, +A]:
    def done(): A
end visitor

package org.finos.morphir.ir

object visitor:
  abstract class MorphirVisitor[+TAttr, +VAttr, +A](downstream: MorphirVisitor[TAttr, VAttr, A]):
    def visitString(value: String): A
    def visitInt(value: Int): A
    def visitType(): TypeVisitor[TAttr, A]

  abstract class TypeVisitor[+TAttr, +A]:
    def done(): A

  abstract class UnitTypeVisitor[+TAttr, +A](downstream: UnitTypeVisitor[TAttr, A]):
    def visitAttributes[TAttr1 >: TAttr](): AttributesVisitor[TAttr1, A]
    def done: A

  abstract class AttributesVisitor[TAttr, +A](downstream: AttributesVisitor[TAttr, A]):
    def visit(attributes: TAttr): Unit
    def done(): A

  object AttributesVisitor:
    def apply[TAttr, A](f: TAttr => A): AttributesVisitor[TAttr, A] =
      var stack = scala.collection.mutable.Stack.empty[() => A]
      new AttributesVisitor[TAttr, A](null):
        def visit(attributes: TAttr): Unit = stack.push(() => f(attributes))
        def done(): A                      = stack.pop()()

    def any[TAttr](downstream: AttributesVisitor[TAttr, Any]): AttributesVisitor[TAttr, Any] =
      new AttributesVisitor[TAttr, Any](downstream):
        def visit(attributes: TAttr): Unit = downstream.visit(attributes)
        def done(): Any                    = downstream.done()

    def unit[TAttr](downstream: AttributesVisitor[TAttr, Unit]): AttributesVisitor[TAttr, Unit] =
      new AttributesVisitor[TAttr, Unit](downstream):
        def visit(attributes: TAttr): Unit = downstream.visit(attributes)
        def done(): Unit                   = downstream.done()
end visitor

class ZioJsonParser {
  import visitor.MorphirVisitor
  import zio.json._
  def dispatch[TAttr, VAttr, A](string: String, visitor: MorphirVisitor[TAttr, VAttr, A]): A = ???
  ???
}

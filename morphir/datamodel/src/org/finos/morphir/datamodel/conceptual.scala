package org.finos.morphir.datamodel

object conceptual {
  trait Ctx {}

  trait Visitor[+T] {
    def visitArray(ctx:Ctx):ArrayVisitor[T]
    def visitBoolean(ctx:Ctx, value: Boolean): T
    def visitObject(ctx: Ctx): RecordVisitor[T]
    def visitString(ctx: Ctx, text: CharSequence): T
  }

  trait RecordVisitor[+T] {
    def visitFieldLabel(ctx: Ctx, label: Label): Unit
    def subVisitor(ctx: Ctx): Visitor[_]
    def visitValue(ctx: Ctx, value: Any): Unit
    def visitEnd(): T
  }

  trait ArrayVisitor[+T] {
    def visitIndex(ctx: Ctx, idx: Int): Unit
    def subVisitor(): Visitor[_]
    def visitValue(ctx: Ctx, value: Any): Unit
    def visitEnd(): T
  }

  trait Printer extends Visitor[scala.Unit] {
    override def visitBoolean(ctx: Ctx, value: Boolean): Unit = ???
  }
}

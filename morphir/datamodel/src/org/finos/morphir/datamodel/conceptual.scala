package org.finos.morphir.datamodel

object conceptual {
  trait Ctx {
  
  }

  trait Visitor[+T] {
    def visitObject(ctx:Ctx):RecordVisitor[T]
    def visitString(ctx:Ctx, text:CharSequence):T  
  }

  trait RecordVisitor[+T] {
    def visitFieldLabel(ctx:Ctx, label:Label):Unit
    def subVisitor(ctx:Ctx):Visitor[_]
    def visitValue(ctx:Ctx, value:Any):Unit
    def visitEnd():T
  }

  trait ArrayVisitor[+T]{
    def visitIndex(ctx: Ctx, idx: Int): Unit
    def subVisitor(): Visitor[_]
    def visitValue(ctx: Ctx, value: Any): Unit
    def visitEnd(): T
  }
}

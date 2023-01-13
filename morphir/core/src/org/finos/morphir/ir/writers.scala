package org.finos
package morphir
package ir

import upickle.core.{Annotator, Visitor}
import ir.{Types => T}

trait IRWriters extends IRValueWriters { self: Annotator => }

trait IRValueWriters extends IRTypeWriters { self: Annotator => }

trait IRTypeWriters extends NamingWriters { self: Annotator =>

  implicit def UnitTypeWriter[A: Writer]: Writer[T.Type.Unit[A]] = new Writer[T.Type.Unit[A]] {
    def write0[R](out: Visitor[_, R], v: T.Type.Unit[A]): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Unit", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitEnd(-1)
    }
  }
}

trait NamingWriters extends upickle.implicits.Writers { self: Annotator =>
  implicit val NameWriter: Writer[Names.Name] = new Writer[Names.Name] {
    def write0[R](out: Visitor[_, R], v: Names.Name): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { str =>
        ctx.visitValue(ctx.subVisitor.visitString(str, -1), -1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val PathWriter: Writer[Paths.Path] = new Writer[Paths.Path] {
    def write0[R](out: Visitor[_, R], v: Paths.Path): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { name =>
        ctx.visitValue(implicitly[Writer[Names.Name]].write(ctx.subVisitor, name), -1)
      }
      ctx.visitEnd(-1)
    }
  }
}


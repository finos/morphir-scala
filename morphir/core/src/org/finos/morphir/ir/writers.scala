package org.finos
package morphir
package ir

import upickle.core.{Annotator, Visitor}
import Type.Type
import ir.Type as T

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

  implicit def VariableTypeWriter[A: Writer]: Writer[T.Type.Variable[A]] = new Writer[T.Type.Variable[A]] {
    override def write0[R](out: Visitor[_, R], v: Type.Variable[A]): R = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Variable", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }
}

trait NamingWriters extends upickle.implicits.Writers { self: Annotator =>
  implicit val NameWriter: Writer[Name.Name] = new Writer[Name.Name] {
    def write0[R](out: Visitor[_, R], v: Name.Name): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { str =>
        ctx.visitValue(ctx.subVisitor.visitString(str, -1), -1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val PathWriter: Writer[Path.Path] = new Writer[Path.Path] {
    def write0[R](out: Visitor[_, R], v: Path.Path): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { name =>
        ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, name), -1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val PackageNameWriter: Writer[Package.PackageName] =
    implicitly[Writer[Path.Path]].comap[Package.PackageName](_.toPath)
}

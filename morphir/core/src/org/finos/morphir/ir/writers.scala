package org.finos
package morphir
package ir

import upickle.core.{Annotator, Visitor}
import Type.Type
import ir.Type as T

trait IRWriters extends IRValueWriters { self: Annotator => }

trait IRValueWriters extends IRTypeWriters { self: Annotator => }

trait IRTypeWriters extends NamingWriters { self: Annotator =>

  implicit def ExtensibleRecordTypeWriter[A: Writer]: Writer[T.Type.ExtensibleRecord[A]] =
    new Writer[T.Type.ExtensibleRecord[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Type.ExtensibleRecord[A]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("ExtensibleRecord", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.name), -1)
        ctx.visitValue(implicitly[Writer[List[T.Field[A]]]].write(ctx.subVisitor, v.fields), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def FunctionTypeWriter[A: Writer]: Writer[T.Type.Function[A]] = new Writer[T.Type.Function[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Function[A]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Function", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.argumentType), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.returnType), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def RecordTypeWriter[A: Writer]: Writer[T.Type.Record[A]] = new Writer[T.Type.Record[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Record[A]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Record", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[List[T.Field[A]]]].write(ctx.subVisitor, v.fields), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ReferenceTypeWriter[A: Writer]: Writer[T.Type.Reference[A]] = new Writer[T.Type.Reference[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Reference[A]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Reference", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[FQName.FQName]].write(ctx.subVisitor, v.typeName), -1)
      ctx.visitValue(implicitly[Writer[List[Type[A]]]].write(ctx.subVisitor, v.typeParams), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def TupleTypeWriter[A: Writer]: Writer[T.Type.Tuple[A]] = new Writer[T.Type.Tuple[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Tuple[A]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Tuple", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[List[Type[A]]]].write(ctx.subVisitor, v.elements), -1)
      ctx.visitEnd(-1)
    }
  }

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

  implicit def TypeWriter[A: Writer]: Writer[Type[A]] = new Writer[Type[A]] {
    override def write0[R](out: Visitor[_, R], v: Type[A]): R = v match {
      case r: T.Type.ExtensibleRecord[A] => ExtensibleRecordTypeWriter.write0(out, r)
      case r: T.Type.Function[A]         => FunctionTypeWriter.write0(out, r)
      case r: T.Type.Record[A]           => RecordTypeWriter.write0(out, r)
      case r: T.Type.Reference[A]        => ReferenceTypeWriter.write0(out, r)
      case r: T.Type.Tuple[A]            => TupleTypeWriter.write0(out, r)
      case r: T.Type.Unit[A]             => UnitTypeWriter.write0(out, r)
      case r: T.Type.Variable[A]         => VariableTypeWriter.write0(out, r)
    }
  }

  implicit def FieldTypeWriter[A: Writer]: Writer[T.Field[A]] = new Writer[T.Field[A]] {
    override def write0[R](out: Visitor[_, R], v: T.Field[A]): R = {
      val ctx = out.visitObject(2, true, -1).narrow
      ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "name"), -1)
      ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "tpe"), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.tpe), -1)
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

  implicit val QNameWriter: Writer[QName.QName] = new Writer[QName.QName] {
    def write0[R](out: Visitor[_, R], v: QName.QName): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(implicitly[Writer[Path.Path]].write(ctx.subVisitor, v.modulePath), -1)
      ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.localName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit val FQNameWriter: Writer[FQName.FQName] = new Writer[FQName.FQName] {
    def write0[R](out: Visitor[_, R], v: FQName.FQName): R = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(implicitly[Writer[Package.PackageName]].write(ctx.subVisitor, v.packagePath), -1)
      ctx.visitValue(implicitly[Writer[Path.Path]].write(ctx.subVisitor, v.modulePath), -1)
      ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.localName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit val ModuleNameWriter: Writer[Module.ModuleName] = new Writer[Module.ModuleName] {
    def write0[R](out: Visitor[_, R], v: Module.ModuleName): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(implicitly[Writer[Path.Path]].write(ctx.subVisitor, v.namespace.toPath), -1)
      ctx.visitValue(implicitly[Writer[Name.Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }
}

trait LiteralWriters {}

package org.finos.morphir.ir.internal

import zio.Chunk
import zio.prelude.fx.ZPure
import org.finos.morphir.ir.Type.Type

trait TransformType[T, A] extends Transform[T] {
  import Transform._

  protected[this] def transform[R](c: R): Stateful[T, R]       = const(c)
  protected[this] def transformAttribute(c: A): Stateful[T, A] = const(c)

  def of(valueIn: Type[A]): Stateful[T, Type[A]] =
    valueIn match {
      case v: Type.ExtensibleRecord[_] => of(v)
      case v: Type.Function[_]         => of(v)
      case v: Type.Record[_]           => of(v)
      case v: Type.Reference[_]        => of(v)
      case v: Type.Tuple[_]            => of(v)
      case v: Type.Unit[_]             => of(v)
      case v: Type.Variable[_]         => of(v)
    }

  // ====== Leaf Elements ======
  def of(value: Type.Unit[A]): Stateful[T, Type.Unit[A]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield v.copy(attr)

  def of(value: Type.Variable[A]): Stateful[T, Type.Variable[A]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield v.copy(attr)

  // ====== Recursive Elements ======
  def of(value: Type.ExtensibleRecord[A]): Stateful[T, Type.ExtensibleRecord[A]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
      fields <- ofList(v.fields) { field =>
        of(field.data).map(data => field.copy(data = data))
      }
    } yield Type.ExtensibleRecord(attr, v.name, fields)

  def of(value: Type.Function[A]): Stateful[T, Type.Function[A]] =
    for {
      v            <- transform(value)
      attr         <- transformAttribute(v.attributes)
      argumentType <- of(v.argumentType)
      returnType   <- of(v.returnType)
    } yield Type.Function(attr, argumentType, returnType)

  def of(value: Type.Record[A]): Stateful[T, Type.Record[A]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
      fields <- ofList(v.fields) { field =>
        of(field.data).map(data => field.copy(data = data))
      }
    } yield Type.Record(attr, fields)

  def of(value: Type.Reference[A]): Stateful[T, Type.Reference[A]] =
    for {
      v          <- transform(value)
      attr       <- transformAttribute(v.attributes)
      typeParams <- ofList(v.typeParams)(of(_))
    } yield Type.Reference(attr, v.typeName, typeParams)

  def of(value: Type.Tuple[A]): Stateful[T, Type.Tuple[A]] =
    for {
      v      <- transform(value)
      attr   <- transformAttribute(v.attributes)
      fields <- ofList(v.elements)(of(_))
    } yield Type.Tuple(attr, fields)
}

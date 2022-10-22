package org.finos
package morphir
package ir
package internal

import org.finos.morphir.ir.{FQName, Name, NeedsAttributes}
import zio.Chunk

trait TypeConstructorsModule extends TypeModule { module =>
  import module.Type._

  // Extensible record constructors
  def extensibleRecord[A](attributes: A, name: Name, fields: Chunk[Field[Type[A]]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    ExtensibleRecord(attributes, name, fields)

  final def extensibleRecord[A](attributes: A, name: String, fields: Chunk[Field[Type[A]]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields)

  final def extensibleRecord[A](attributes: A, name: String, field: Field[Type[A]], fields: Field[Type[A]]*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), field +: Chunk.fromIterable(fields))

  final def extensibleRecord[A](attributes: A, name: Name, fields: (String, Type[A])*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) =>
      Field(Name.fromString(name), typeExpr)
    })
    ExtensibleRecord(attributes, name, fieldsChunk)
  }

  final def extensibleRecord[A](attributes: A, name: String, fields: (String, Type[A])*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields: _*)

  // Function constructors
  final def function[A](attributes: A, argumentType: Type[A], returnType: Type[A]): Type[A] =
    Function(attributes, argumentType, returnType)

  // Record constructors
  final def record[A](attributes: A, fields: Chunk[Field[Type[A]]])(implicit ev: NeedsAttributes[A]): Type[A] =
    Record(attributes, fields)

  final def reference[A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Reference(attributes, typeName, typeParams)

  final def reference[A](attributes: A, typeName: FQName)(implicit ev: NeedsAttributes[A]): Type[A] =
    Reference(attributes, typeName, Chunk.empty)

  final def reference[A](attributes: A, typeName: FQName, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Reference(attributes, typeName, Chunk.fromIterable(firstTypeParam +: otherTypeParams))

  final def reference[A](attributes: A, typeName: String, typeParams: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]) =
    Reference(attributes, FQName.fromString(typeName), typeParams)

  final def reference[A](attributes: A, typeName: String)(implicit ev: NeedsAttributes[A]) =
    Reference(attributes, FQName.fromString(typeName), Chunk.empty)

  final def reference[A](attributes: A, typeName: String, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      ev: NeedsAttributes[A]
  ) =
    Reference(attributes, FQName.fromString(typeName), firstTypeParam +: Chunk.fromIterable(otherTypeParams))

  // Tuple constructors
  final def emptyTuple[A](attributes: A)(implicit ev: NeedsAttributes[A]): Type[A] =
    Tuple(attributes, Chunk.empty)

  final def tuple[A](attributes: A, elements: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
    Tuple(attributes, elements)

  final def tuple[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
    tuple(attributes, Chunk.fromIterable(elements))

  final def unit[A](attributes: A)(implicit ev: NeedsAttributes[A]): Type[A] = Unit(attributes)

  // Variable constructors
  final def variable[A](attributes: A, name: Name)(implicit ev: NeedsAttributes[A]): Type[A] =
    Variable(attributes, name)

  final def variable[A](attributes: A, name: String)(implicit ev: NeedsAttributes[A]): Type[A] =
    Variable(attributes, Name.fromString(name))
}

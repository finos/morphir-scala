package org.finos.morphir
package ir
package types
package folding

import zio.Chunk
sealed trait Type[+A] extends Product with Serializable { self =>
  import Type._
  final def ??(doc: String): Documented[Type[A]] = Documented(doc, self)
  def attributes: A

  final def foldContext[C, A1 >: A, Z](context: C)(folder: Folder[C, A1, Z]): Z = {
    import folder._
    def loop(in: List[Type[A]]): Z = ???
    loop(List(self))
  }
}
object Type extends TypeConstructors with UnattributedTypeConstructors {
  type FieldT[+A] = Field[Type[A]]
  val FieldT: Field.type = Field

  type UType = Type[Any]
  val UType = Type

  final case class ExtensibleRecord[+A](attributes: A, name: Name, fields: Chunk[Field[Type[A]]]) extends Type[A]
  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])        extends Type[A]
  final case class Record[+A](attributes: A, fields: Chunk[Field[Type[A]]])                       extends Type[A]
  final case class Reference[+A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]])     extends Type[A]
  final case class Tuple[+A](attributes: A, elements: Chunk[Type[A]])                             extends Type[A]
  final case class Unit[+A](attributes: A)                                                        extends Type[A]
  final case class Variable[+A](attributes: A, name: Name)                                        extends Type[A]

  trait Folder[-Context, -Attrib, Z] {
    def extensibleRecord(context: Context, attributes: Attrib, name: Name, fields: Chunk[Field[Z]]): Z
    def function(context: Context, attributes: Attrib, argumentType: Z, returnType: Z): Z
    def record(context: Context, attributes: Attrib, fields: Chunk[Field[Z]]): Z
    def reference(context: Context, attributes: Attrib, typeName: FQName, typeParams: Chunk[Z]): Z
    def tuple(context: Context, attributes: Attrib, elements: Chunk[Z]): Z
    def unit(context: Context, attributes: Attrib): Z
    def variable(context: Context, attributes: Attrib, name: Name): Z
  }
}

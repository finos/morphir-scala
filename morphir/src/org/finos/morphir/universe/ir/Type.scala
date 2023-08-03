package org.finos.morphir.universe.ir
import org.finos.morphir.universe.ir.Type.ExtensibleRecord
import org.finos.morphir.universe.ir.Type.Record
import org.finos.morphir.universe.ir.Type.Reference
import org.finos.morphir.universe.ir.Type.Tuple
import org.finos.morphir.universe.ir.Type.Variable

sealed trait Type[+A] {
  import Type.*

  def map[B](f: A => B): Type[B] = ???
}
object Type {

  def reference[A](attributes: A)(name: FQName, typeParams: List[Type[A]] = List.empty): Reference[A] =
    Reference(attributes, name, typeParams)

  final case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]])   extends Type[A]
  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])   extends Type[A]
  final case class Record[+A](attributes: A, fields: List[Field[A]])                         extends Type[A]
  final case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]]) extends Type[A]
  final case class Tuple[+A](attributes: A, elements: List[Type[A]])                         extends Type[A]
  final case class Unit[+A](attributes: A)                                                   extends Type[A]
  final case class Variable[+A](attributes: A, name: Name)                                   extends Type[A]

  final case class Field[+A](name: Name, tpe: Type[A]) {
    def map[B](f: A => B): Field[B] = Field(name, tpe.map(f))
  }
}

object TypeVisitorUsage {
  // val visitor = TypeVisitor.stateful(()) {
  //   case Type.Unit(_) => ???
  //   case _            => ???
  // }

  // visitor[scala.Unit](Type.Unit(()))
}

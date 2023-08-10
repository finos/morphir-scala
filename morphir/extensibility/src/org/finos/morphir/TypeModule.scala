package org.finos.morphir
import org.finos.morphir.naming._
trait TypeModule { self =>

  sealed trait Type[+A] { self =>
    import Type._

    def attributes: A

    def map[B](f: A => B): Type[B] = ???
  }
  object Type {

    type Field[+A] = FieldT[Type[A]]
    val Field = FieldT

    def reference[A](attributes: A)(name: FQName, typeParams: List[Type[A]] = List.empty): Reference[A] =
      Reference(attributes, name, typeParams)

    sealed case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]]) extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A]) extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[A]])                       extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]])
        extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]]) extends Type[A]
    sealed case class Unit[+A](attributes: A)                           extends Type[A]
    sealed case class Variable[+A](attributes: A, name: Name)           extends Type[A]

  }

  sealed case class FieldT[+T](name: Name, data: T) {
    @inline def tpe: T               = data
    def map[B](f: T => B): FieldT[B] = copy(data = f(data))
  }
}

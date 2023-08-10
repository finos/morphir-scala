package org.finos.morphir

trait TypeModule {
  sealed trait Type[+A] {
    import Type._

    def map[B](f: A => B): Type[B] = ???
  }
  object Type {

    def reference[A](attributes: A)(name: FQNameModule#FQName, typeParams: List[Type[A]] = List.empty): Reference[A] =
      Reference(attributes, name, typeParams)

    sealed case class ExtensibleRecord[+A](attributes: A, name: NameModule#Name, fields: List[Field[A]]) extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])            extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[A]])                                  extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQNameModule#FQName, typeParams: List[Type[A]])
        extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]])  extends Type[A]
    sealed case class Unit[+A](attributes: A)                            extends Type[A]
    sealed case class Variable[+A](attributes: A, name: NameModule#Name) extends Type[A]

    sealed case class Field[+A](name: NameModule#Name, tpe: Type[A]) {
      def map[B](f: A => B): Field[B] = Field(name, tpe.map(f))
    }
  }

  sealed trait TypeSpecification
  object TypeSpecification {}

  sealed trait TypeDefinition
}

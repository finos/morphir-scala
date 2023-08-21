package org.finos.morphir.internal

trait TypeInfoModule { self: TypeModule with TypeSpecModule with TypeDefModule =>

  sealed trait TypeInfo[+A] {
    def tpe: Type[A]
  }

  object TypeInfo {
    def fromType[A](tpe: Type[A]): TypeInfo[A] = TypeOnly(tpe)

    sealed case class TypeOnly[+A](tpe: Type[A])                                extends TypeInfo[A]
    sealed case class TypeAndSpec[+A](tpe: Type[A], spec: TypeSpecification[A]) extends TypeInfo[A]
    sealed case class Full[+A](tpe: Type[A], spec: TypeSpecification[A], definition: TypeDefinition[A])
        extends TypeInfo[A]
  }

}

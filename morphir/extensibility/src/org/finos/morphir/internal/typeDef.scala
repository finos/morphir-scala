package org.finos.morphir.internal

import org.finos.morphir.naming.*
trait TypeDefModule { self: AccessControlledModule with TypeModule with TypeSpecModule =>

  import TypeDefinition.*
  sealed trait TypeDefinition[+Attribs] { self =>
    def map[B](f: Attribs => B): TypeDefinition[B] = self match {
      case TypeAliasDefinition(typeParams, typeExp) => TypeAliasDefinition(typeParams, typeExp.map(f))
      case CustomTypeDefinition(typeParams, ctors)  => CustomTypeDefinition(typeParams, ctors.map(_.map(f)))
    }
  }

  object TypeDefinition {
    sealed case class TypeAliasDefinition[+A](typeParams: Vector[Name], typeExpr: Type[A]) extends TypeDefinition[A]
    sealed case class CustomTypeDefinition[+A](typeParams: Vector[Name], ctors: AccessControlled[TypeConstructors[A]])
        extends TypeDefinition[A]
  }
}

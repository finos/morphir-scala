package org.finos.morphir.internal

import org.finos.morphir.naming._
import org.finos.morphir.universe.ir.{AccessControlled, Field, Type}

trait TypeDefModule { self: TypeSpecModule =>

  import TypeDefinition.*
  sealed trait TypeDefinition[+Attribs] { self =>
    def map[B](f: Attribs => B): TypeDefinition[B] = self match {
      case TypeAliasDefinition(typeParams, typeExp) => TypeAliasDefinition(typeParams, typeExp.map(f))
      case CustomTypeDefinition(typeParams, ctors)  => CustomTypeDefinition(typeParams, ctors.map(_.map(f)))
    }
  }

  object TypeDefinition {
    @inline def alias[A](typeParams: Vector[Name], typeExpr: Type[A]): TypeDefinition[A] =
      TypeAliasDefinition(typeParams, typeExpr)

    @inline def alias[A](typeExpr: Type[A]): TypeDefinition[A] = alias(Vector.empty, typeExpr)

    @inline def custom[A](typeParams: Vector[Name], ctors: TypeConstructors[A]): TypeDefinition[A] =
      CustomTypeDefinition(typeParams, AccessControlled.publicAccess(ctors))

    def custom[A](typeParams: Vector[Name], ctors: AccessControlled[TypeConstructors[A]]): TypeDefinition[A] =
      CustomTypeDefinition(typeParams, ctors)

    sealed case class TypeAliasDefinition[+A](typeParams: Vector[Name], typeExpr: Type[A]) extends TypeDefinition[A]
    sealed case class CustomTypeDefinition[+A](typeParams: Vector[Name], ctors: AccessControlled[TypeConstructors[A]])
        extends TypeDefinition[A]
  }
}

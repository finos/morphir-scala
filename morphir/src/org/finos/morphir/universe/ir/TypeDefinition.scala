package org.finos.morphir.universe.ir

import org.finos.morphir.naming._

sealed trait TypeDefinition[+A] { self =>
  def map[B](f: A => B): TypeDefinition[B] = self match {
    case TypeDefinition.TypeAliasDefinition(_, _)  => ???
    case TypeDefinition.CustomTypeDefinition(_, _) => ???
  }
}

object TypeDefinition {
  final case class TypeAliasDefinition[+A](typeParams: Vector[Name], typeExpr: Type[A]) extends TypeDefinition[A]
  final case class CustomTypeDefinition[+A](typeParams: Vector[Name], ctors: AccessControlled[TypeConstructors[A]])
      extends TypeDefinition[A]
}

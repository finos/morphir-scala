package org.finos.morphir.universe.ir

sealed trait TypeDefinition[+A]
object TypeDefinition {
  final case class TypeAliasDefinition[+A](typeParams: Vector[Name], typeExpr: Type[A]) extends TypeDefinition[A]
  final case class CustomTypeDefinition[+A](typeParams: Vector[Name], ctors: AccessControlled[TypeConstructors[A]])
      extends TypeDefinition[A]
}

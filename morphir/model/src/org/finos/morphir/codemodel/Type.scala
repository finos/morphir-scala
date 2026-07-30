package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Chunk
import kyo.Schema

enum Type derives Schema {
  case Variable(attributes: TypeAttributes, name: Name)
  case Reference(attributes: TypeAttributes, fqName: FQName, args: Chunk[Type])
  case Tuple(attributes: TypeAttributes, elements: Chunk[Type])
  case Record(attributes: TypeAttributes, fields: Chunk[Field])
  case ExtensibleRecord(attributes: TypeAttributes, variable: Name, fields: Chunk[Field])
  case Function(attributes: TypeAttributes, argumentType: Type, returnType: Type)
  case Unit(attributes: TypeAttributes)
}

final case class Field(name: Name, fieldType: Type) derives Schema

/**
 * A named, typed slot: a function/value parameter or a constructor argument. Shared by `ValueDefinitionBody`,
 * `ValueSpecification` (in `Expr.scala`) and `Constructor.args` below, so it lives here alongside `Field` rather than
 * duplicated per use site. Replaces the raw `(Name, Type)` tuple those fields used to carry, which kyo-schema would
 * otherwise encode as meaningless `{"_1": ..., "_2": ...}`.
 */
final case class Parameter(name: Name, tpe: Type) derives Schema

enum TypeSpecification derives Schema {
  case TypeAliasSpecification(params: Chunk[Name], body: Type)
  case OpaqueTypeSpecification(params: Chunk[Name])
  case CustomTypeSpecification(params: Chunk[Name], constructors: Chunk[Constructor])
  case DerivedTypeSpecification(params: Chunk[Name], details: DerivedTypeSpecificationDetails)
}

final case class Constructor(name: Name, args: Chunk[Parameter]) derives Schema

final case class DerivedTypeSpecificationDetails(
    baseType: Type,
    fromBaseType: FQName,
    toBaseType: FQName
) derives Schema

enum TypeDefinition derives Schema {
  case CustomTypeDefinition(params: Chunk[Name], access: AccessControlled[Chunk[Constructor]])
  case TypeAliasDefinition(params: Chunk[Name], body: Type)
  // IncompleteTypeDefinition is for internal tracking, might not be needed in core AST yet, but included for completeness with spec
  case IncompleteTypeDefinition(params: Chunk[Name], incompleteness: Incompleteness, partialBody: Option[Type])
}

enum Incompleteness derives Schema {
  case Hole(reason: HoleReason)
  case Draft(notes: Option[String])
}

enum HoleReason derives Schema {
  case UnresolvedReference(target: FQName)
  case DeletedDuringRefactor(txId: String)
  case TypeMismatch(expected: String, found: String)
}

enum Access derives Schema {
  case Public
  case Private
}

final case class AccessControlled[+A](access: Access, value: A) derives Schema

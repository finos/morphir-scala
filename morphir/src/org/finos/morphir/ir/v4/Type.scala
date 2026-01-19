package org.finos.morphir
package ir
package v4

import org.finos.morphir.naming._
import zio.Chunk

enum Type {
  case Variable(attributes: TypeAttributes, name: Name)
  case Reference(attributes: TypeAttributes, fqName: FQName, args: Chunk[Type])
  case Tuple(attributes: TypeAttributes, elements: Chunk[Type])
  case Record(attributes: TypeAttributes, fields: Chunk[Field])
  case ExtensibleRecord(attributes: TypeAttributes, variable: Name, fields: Chunk[Field])
  case Function(attributes: TypeAttributes, argumentType: Type, returnType: Type)
  case Unit(attributes: TypeAttributes)
}

final case class Field(name: Name, fieldType: Type)

enum TypeSpecification {
  case TypeAliasSpecification(params: Chunk[Name], body: Type)
  case OpaqueTypeSpecification(params: Chunk[Name])
  case CustomTypeSpecification(params: Chunk[Name], constructors: Chunk[Constructor])
  case DerivedTypeSpecification(params: Chunk[Name], details: DerivedTypeSpecificationDetails)
}

final case class Constructor(name: Name, args: Chunk[(Name, Type)])

final case class DerivedTypeSpecificationDetails(
    baseType: Type,
    fromBaseType: FQName,
    toBaseType: FQName
)

enum TypeDefinition {
  case CustomTypeDefinition(params: Chunk[Name], access: AccessControlled[Chunk[Constructor]])
  case TypeAliasDefinition(params: Chunk[Name], body: Type)
  // IncompleteTypeDefinition is for internal tracking, might not be needed in core AST yet, but included for completeness with spec
  case IncompleteTypeDefinition(params: Chunk[Name], incompleteness: Incompleteness, partialBody: Option[Type])
}

enum Incompleteness {
  case Hole(reason: HoleReason)
  case Draft(notes: Option[String])
}

enum HoleReason {
  case UnresolvedReference(target: FQName)
  case DeletedDuringRefactor(txId: String)
  case TypeMismatch(expected: String, found: String)
}

enum Access {
  case Public
  case Private
}

final case class AccessControlled[+A](access: Access, value: A)

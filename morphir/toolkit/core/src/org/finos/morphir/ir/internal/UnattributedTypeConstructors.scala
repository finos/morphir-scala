package org.finos
package morphir
package ir
package internal

import zio.Chunk
import org.finos.morphir.ir.{FQName, Name}

trait UnattributedTypeConstructorsModule extends TypeModule { module =>
  import module.Type._

  final def curriedFunction(paramTypes: List[UType], returnType: UType): UType = {
    def curry(args: List[UType]): UType = args match {
      case Nil                    => returnType
      case firstArg :: restOfArgs => function(firstArg, curry(restOfArgs))
    }
    curry(paramTypes)
  }

  final def extensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), name, fields)

  final def extensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), Name.fromString(name), fields)

  final def extensibleRecord(name: Name, fields: (String, UType)*): UType = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) => Field(Name.fromString(name), typeExpr) })
    ExtensibleRecord((), name, fieldsChunk)
  }

  final def extensibleRecord(name: String, fields: (String, UType)*): UType =
    extensibleRecord(Name.fromString(name), fields: _*)

  final def extensibleRecordWithFields(name: Name, fields: Field[UType]*): UType =
    ExtensibleRecord((), name, Chunk.fromIterable(fields))

  final def extensibleRecordWithFields(name: String, fields: Field[UType]*): UType =
    ExtensibleRecord((), Name.fromString(name), Chunk.fromIterable(fields))

  final def function(argumentType: UType, returnType: UType): UType =
    Function((), argumentType, returnType)

  final def record(fields: Chunk[Field[UType]]): UType =
    Record((), fields)

  final def record(field: Field[UType], fields: Field[UType]*): UType =
    Record((), field +: Chunk.fromIterable(fields))

  final def record(fields: (String, UType)*): UType =
    Record((), Chunk.fromIterable(fields.map { case (name, typeExpr) => Field(Name.fromString(name), typeExpr) }))

  final def reference(typeName: FQName, typeParams: Chunk[UType]): UType =
    Reference((), typeName, typeParams)

  final def reference(typeName: FQName, typeParams: UType*): UType =
    Reference((), typeName, Chunk.fromIterable(typeParams))

  final def reference(typeName: String, typeParams: Chunk[UType]): UType =
    Reference((), FQName.fromString(typeName), typeParams)

  final def reference(typeName: String, typeParams: UType*): UType =
    Reference((), FQName.fromString(typeName), Chunk.fromIterable(typeParams))

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: UType*): UType =
    Reference((), FQName.fqn(packageName, moduleName, typeName), Chunk.fromIterable(typeParams))

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: Chunk[UType]): UType =
    Reference((), FQName.fqn(packageName, moduleName, typeName), typeParams)

  final def tuple(elements: UType*): UType =
    Tuple((), Chunk.fromIterable(elements))

  final def tuple(elements: Chunk[UType]): UType =
    Tuple((), elements)

  final val unit: UType = Unit(())

  final def variable(name: Name): UType =
    Variable((), name)

  final def variable(name: String): UType =
    Variable((), Name.fromString(name))
}

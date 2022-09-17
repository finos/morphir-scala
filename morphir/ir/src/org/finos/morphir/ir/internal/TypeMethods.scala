package org.finos.morphir.ir.internal

import org.finos.morphir.ir.internal.naming.{FQName, Name}
import Type._

private[ir] abstract class TypeMethods[+Attr] extends TypeBase[Attr]:
  self: Type[Attr] =>

  def fold[Attr1 >: Attr, Z](folder: Folder[Attr1, Z]): Z =
    self match

      case ExtensibleRecord(attributes, variableName, fields) =>
        folder.extensibleRecordCase(
          attributes,
          variableName,
          fields.map { case Type.Field(name, tpe) => (name, tpe.fold(folder)) }
        )

      case Function(attributes, argumentType, returnType) =>
        folder.functionCase(attributes, argumentType.fold(folder), returnType.fold(folder))

      case Record(attributes, fields) =>
        folder.recordCase(
          attributes,
          fields.map { case Type.Field(name, tpe) => (name, tpe.fold(folder)) }
        )

      case Reference(attributes, typeName, typeParameters) =>
        folder.referenceCase(
          attributes,
          typeName,
          typeParameters.map(_.fold(folder))
        )

      case Tuple(attributes, elementTypes) =>
        folder.tupleCase(attributes, elementTypes.map(_.fold(folder)))

      case Unit(attributes) =>
        folder.unitCase(attributes)

      case Variable(attributes, name) =>
        folder.variableCase(attributes, name)
  end fold

  extension [Attr](self: Type.Reference[Attr]) inline def typeArgs: List[Type[Attr]] = self.typeParameters
end TypeMethods

sealed private[ir] abstract class TypeBase[+Attr]:
  def attributes: Attr

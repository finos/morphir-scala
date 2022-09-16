package org.finos.morphir.ir

import org.finos.morphir.ir.naming.{FQName, Name}

object tree:
  sealed abstract class TypeBase[+Attr]:
    def attributes: Attr

  enum Type[+Attr] extends TypeBase[Attr]:
    self =>
    import Type.Folder
    case ExtensibleRecord(attributes: Attr, variableName: Name, fields: List[Type.Field[Attr]])
    case Function(attributes: Attr, argumentType: Type[Attr], returnType: Type[Attr])
    case Record(attributes: Attr, fields: List[Type.Field[Attr]])
    case Reference(attributes: Attr, typeName: FQName, typeParameters: List[Type[Attr]])
    case Tuple(attributes: Attr, elementTypes: List[Type[Attr]])
    case Unit(attributes: Attr)
    case Variable(attributes: Attr, name: Name)

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

  object Type:
    final case class Field[+Attr](name: Name, tpe: Type[Attr]):
      inline def fieldType: Type[Attr] = tpe

    trait Folder[-Attr, Z] {
      def extensibleRecordCase(attributes: Attr, variableName: Name, fields: List[(Name, Z)]): Z
      def functionCase(attributes: Attr, argumentType: Z, returnType: Z): Z
      def recordCase(attributes: Attr, fields: List[(Name, Z)]): Z
      def referenceCase(attributes: Attr, typeName: FQName, typeParameters: List[Z]): Z
      def tupleCase(attributes: Attr, elementTypes: List[Z]): Z
      def unitCase(attributes: Attr): Z
      def variableCase(attributes: Attr, name: Name): Z
    }

    final case class Mapper[Attr, Attr1](f: Attr => Attr1) extends Folder[Attr, Type[Attr1]]:
      def extensibleRecordCase(attributes: Attr, variableName: Name, fields: List[(Name, Type[Attr1])]): Type[Attr1] =
        Type.ExtensibleRecord(f(attributes), variableName, fields.map { case (name, tpe) => Type.Field(name, tpe) })

      def functionCase(attributes: Attr, argumentType: Type[Attr1], returnType: Type[Attr1]): Type[Attr1] =
        Type.Function(f(attributes), argumentType, returnType)

      def recordCase(attributes: Attr, fields: List[(Name, Type[Attr1])]): Type[Attr1] =
        Type.Record(f(attributes), fields.map { case (name, tpe) => Type.Field(name, tpe) })

      def referenceCase(attributes: Attr, typeName: FQName, typeParameters: List[Type[Attr1]]): Type[Attr1] =
        Type.Reference(f(attributes), typeName, typeParameters)

      def tupleCase(attributes: Attr, elementTypes: List[Type[Attr1]]): Type[Attr1] =
        Type.Tuple(f(attributes), elementTypes)

      def unitCase(attributes: Attr): Type[Attr1] =
        Type.Unit(f(attributes))

      def variableCase(attributes: Attr, name: Name): Type[Attr1] =
        Type.Variable(f(attributes), name)

  enum Value[+TAttr, +VAttr]:
    case Unit(attributes: VAttr)

    def fold[Attr1 >: TAttr, Attr2 >: VAttr, Z](folder: Value.Folder[Attr1, Attr2, Z]): Z =
      this match
        case Unit(attributes) =>
          folder.unitCase(attributes)

  object Value:
    trait Folder[-TAttr, -VAttr, Z] {
      def unitCase(attributes: VAttr): Z
    }

  final case class Module[+TAttr, +VAttr](
      name: FQName,
      types: List[Type[TAttr]],
      values: List[Value[TAttr, VAttr]]
  ) {

    def fold[Z](folder: Module.Folder[TAttr, VAttr, Z]): Z =
      folder.moduleCase(
        name,
        types.map(_.fold(folder.typeCase)),
        values.map(_.fold(folder.valueCase))
      )
  }

  object Module:

    trait Folder[-TAttr, -VAttr, Z] {
      def moduleCase(name: FQName, types: List[Z], values: List[Z]): Z
      def typeCase: Type.Folder[TAttr, Z]
      def valueCase: Value.Folder[TAttr, VAttr, Z]
    }

  extension [Attr](self: Type.Reference[Attr]) inline def typeArgs: List[Type[Attr]] = self.typeParameters

/**
 * Source => IR Bytes => IR => MIR
 */

package org.finos.morphir.ir

import org.finos.morphir.ir.naming.{FQName, Name}

object tree:
  sealed abstract class TypeBase[+Attr]:
    def attributes: Attr

  enum Type[+Attr] extends TypeBase[Attr]:
    case ExtensibleRecord(attributes: Attr, variableName: Name, fields: List[Type.Field[Attr]])
    case Function(attributes: Attr, argumentType: Type[Attr], returnType: Type[Attr])
    case Record(attributes: Attr, fields: List[Type.Field[Attr]])
    case Reference(attributes: Attr, typeName: FQName, typeParameters: List[Type[Attr]])
    case Tuple(attributes: Attr, elementTypes: List[Type[Attr]])
    case Unit(attributes: Attr)
    case Variable(attributes: Attr, name: Name)

  object Type:
    final case class Field[+Attr](name: Name, tpe: Type[Attr]):
      inline def fieldType: Type[Attr] = tpe

  enum Value[+TAttr, +VAttr]:
    case Unit(attributes: VAttr)

  extension [Attr](self: Type.Reference[Attr]) inline def typeArgs: List[Type[Attr]] = self.typeParameters


/**
  *  Source => IR 
  *  Bytes => IR => MIR
  */

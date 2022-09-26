package org.finos.morphir.ir.internal

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.internal.naming.FQName
import Type._

private[ir] enum Type[+Attr] extends TypeMethods[Attr]:
  self =>
  case ExtensibleRecord(attributes: Attr, variableName: Name, fields: List[Type.Field[Attr]])
  case Function(attributes: Attr, argumentType: Type[Attr], returnType: Type[Attr])
  case Record(attributes: Attr, fields: List[Type.Field[Attr]])
  case Reference(attributes: Attr, typeName: FQName, typeParameters: List[Type[Attr]])
  case Tuple(attributes: Attr, elementTypes: List[Type[Attr]])
  case Unit(attributes: Attr)
  case Variable(attributes: Attr, name: Name)

end Type

private[ir] object Type:
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
end Type

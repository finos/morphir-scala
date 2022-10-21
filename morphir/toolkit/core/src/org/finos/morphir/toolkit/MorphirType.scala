package org.finos.morphir
package toolkit
import zio._
import org.finos.morphir.toolkit.io.MorphirTypeWriter

sealed trait MorphirType { self =>
  import MorphirType.{Unit => UnitType, _}
  final def ??(doc: String): Documented[MorphirType] = Documented(doc, self)

  def attributes: Attributes

  def write[Context](context: Context)(writer: MorphirTypeWriter[Context]): Unit =
    self match {
      case ExtensibleRecord(attributes, name, fields)                 => ???
      case MorphirType.Function(attributes, argumentType, returnType) => ???
      case Record(attributes, fields)                                 => ???
      case Reference(attributes, typeName, typeParams)                => ???
      case Tuple(attributes, elements)                                => ???
      case MorphirType.Unit(attributes)                               => writer.writeUnit(context, attributes)
      case Variable(name, attributes)                                 => writer.writeVariable(context, attributes, name)
    }

  def writeZIO[Context: Tag](writer: MorphirTypeWriter[Context]): ZIO[Context, Throwable, Unit] =
    self match {
      case ExtensibleRecord(attributes, name, fields)     => ???
      case Function(attributes, argumentType, returnType) => ???
      case Record(attributes, fields)                     => ???
      case Reference(attributes, typeName, typeParams)    => ???
      case Tuple(attributes, elements)                    => ???
      case UnitType(attributes)                           => writer.writeUnitZIO(attributes)
      case Variable(name, attributes)                     => writer.writeVariableZIO(attributes, name)
    }
}

object MorphirType {

  type Field = ir.Type.Field[MorphirType]
  val Field = ir.Type.Field

  final case class Constructor(toMap: Map[Name, Chunk[(Name, MorphirType)]])

  final case class ExtensibleRecord(attributes: Attributes, name: Name, fields: Chunk[Field]) extends MorphirType
  final case class Function(attributes: Attributes, argumentType: MorphirType, returnType: MorphirType)
      extends MorphirType
  final case class Record(attributes: Attributes, fields: Chunk[Field]) extends MorphirType
  final case class Reference(attributes: Attributes, typeName: FQName, typeParams: Chunk[MorphirType])
      extends MorphirType
  final case class Tuple(attributes: Attributes, elements: Chunk[MorphirType]) extends MorphirType
  final case class Unit(attributes: Attributes)                                extends MorphirType
  final case class Variable(name: Name, attributes: Attributes)                extends MorphirType

  final implicit class FieldOps(private val self: Field) extends AnyVal {
    def fieldType: MorphirType = self.data
  }

  sealed trait Specification extends Product with Serializable { self =>
    def ??(doc: String): Documented[Specification] = Documented(doc, self)
  }

  object Specification {
    final case class TypeAliasSpecification(typeParams: Chunk[Name], expr: MorphirType)  extends Specification
    final case class OpaqueTypeSpecification(typeParams: Chunk[Name], expr: MorphirType) extends Specification
    final case class CustomTypeSpecification()
  }
}

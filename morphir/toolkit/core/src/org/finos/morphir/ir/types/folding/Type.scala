package org.finos.morphir
package ir
package types
package folding

import scala.annotation.tailrec
import zio.Chunk

sealed trait Type[+A] extends Product with Serializable { self =>
  import Type.{Unit => UnitType, _}

  final def ??(doc: String): Documented[Type[A]] = Documented(doc, self)

  def attributes: A

  final def foldContext[C, A1 >: A, Z](context: C)(folder: Folder[C, A1, Z]): Z = {
    import folder._
    sealed trait TypeCase {
      def attributes: A1
    }
    case class ExtensibleRecordCase(attributes: A1, name: Name, fieldSize: Int)     extends TypeCase
    case class FunctionCase(attributes: A1)                                         extends TypeCase
    case class RecordCase(attributes: A1, fieldSize: Int)                           extends TypeCase
    case class ReferenceCase(attributes: A1, typeName: FQName, typeParamsSize: Int) extends TypeCase
    case class TupleCase(attributes: A1, arity: Int)                                extends TypeCase

    @tailrec
    def loop(in: List[Type[A]], out: List[Either[TypeCase, Z]]): List[Z] =
      in match {
        case ExtensibleRecord(attributes, name, fields) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          loop(fieldTypeExprs ++ types, Left(ExtensibleRecordCase(attributes, name, fields.size)) :: out)
        case Function(attributes, argumentType, returnType) :: types =>
          loop(argumentType :: returnType :: types, Left(FunctionCase(attributes)) :: out)
        case Record(attributes, fields) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          loop(fieldTypeExprs ++ types, Left(RecordCase(attributes, fields.size)) :: out)
        case Reference(attributes, typeName, typeParams) :: types =>
          loop(typeParams.toList ++ types, Left(ReferenceCase(attributes, typeName, typeParams.size)) :: out)
        case Tuple(attributes, elements) :: types =>
          loop(elements.toList ++ types, Left(TupleCase(attributes, elements.size)) :: out)
        case UnitType(attributes) :: types => loop(types, Right(unitCase(context, attributes)) :: out)
        case Variable(attributes, name) :: types =>
          loop(types, Right(variableCase(context, attributes, name)) :: out)
        case Nil =>
          out.foldLeft[List[Z]](List.empty) {
            case (acc, Right(typ)) =>
              typ :: acc
            case (acc, Left(ExtensibleRecordCase(attributes, name, fieldSize))) => ???
            case (acc, Left(FunctionCase(attributes))) =>
              val argumentType :: returnType :: rest = (acc: @unchecked)
              functionCase(context, attributes, argumentType, returnType) :: rest
            case (acc, Left(RecordCase(attributes, fieldSize)))         => ???
            case (acc, Left(ReferenceCase(attributes, typeName, size))) =>
              // TODO: Is there a better way to do this?
              val typeParams = Chunk.fromIterable(acc.take(size))
              val rest       = acc.drop(size)
              referenceCase(context, attributes, typeName, typeParams) :: rest
            case (acc, Left(TupleCase(attributes, arity))) =>
              val elements = Chunk.fromIterable(acc.take(arity))
              val rest     = acc.drop(arity)
              tupleCase(context, attributes, elements) :: rest
          }
      }
    loop(List(self), List.empty).head
  }

  final def satisfies(check: PartialFunction[Type[A], Boolean]): Boolean =
    check.lift(self).getOrElse(false)

  final def size: Int = foldContext(())(Type.Folder.Size)

  final override def toString: String = foldContext(())(Type.Folder.ToString)
}
object Type extends TypeConstructors with UnattributedTypeConstructors {
  type FieldT[+A] = Field[Type[A]]
  val FieldT: Field.type = Field

  type UType = Type[Any]
  val UType = Type

  final case class ExtensibleRecord[+A](attributes: A, name: Name, fields: Chunk[Field[Type[A]]]) extends Type[A]
  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])        extends Type[A]
  final case class Record[+A](attributes: A, fields: Chunk[Field[Type[A]]])                       extends Type[A]
  final case class Reference[+A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]])     extends Type[A]
  final case class Tuple[+A](attributes: A, elements: Chunk[Type[A]])                             extends Type[A]
  final case class Unit[+A](attributes: A)                                                        extends Type[A]
  final case class Variable[+A](attributes: A, name: Name)                                        extends Type[A]
  object Variable {
    def apply(name: String): Variable[Any]             = Variable((), Name.fromString(name))
    def apply(name: Name): Variable[Any]               = Variable((), name)
    def apply[A](attributes: A, name: String): Type[A] = Variable(attributes, Name.fromString(name))
  }

  trait Folder[-Context, -Attrib, Z] {
    def extensibleRecordCase(context: Context, attributes: Attrib, name: Name, fields: Chunk[Field[Z]]): Z
    def functionCase(context: Context, attributes: Attrib, argumentType: Z, returnType: Z): Z
    def recordCase(context: Context, attributes: Attrib, fields: Chunk[Field[Z]]): Z
    def referenceCase(context: Context, attributes: Attrib, typeName: FQName, typeParams: Chunk[Z]): Z
    def tupleCase(context: Context, attributes: Attrib, elements: Chunk[Z]): Z
    def unitCase(context: Context, attributes: Attrib): Z
    def variableCase(context: Context, attributes: Attrib, name: Name): Z
  }

  object Folder {
    object Size extends Folder[Any, Any, Int] {
      def extensibleRecordCase(context: Any, attributes: Any, name: Name, fields: Chunk[Field[Int]]): Int =
        fields.map(_.data).sum + 1
      def functionCase(context: Any, attributes: Any, argumentType: Int, returnType: Int): Int =
        argumentType + returnType + 1
      def recordCase(context: Any, attributes: Any, fields: Chunk[Field[Int]]): Int =
        fields.map(_.data).sum + 1
      def referenceCase(context: Any, attributes: Any, typeName: FQName, typeParams: Chunk[Int]): Int =
        typeParams.sum + 1
      def tupleCase(context: Any, attributes: Any, elements: Chunk[Int]): Int =
        elements.sum + 1
      def unitCase(context: Any, attributes: Any): Int                 = 1
      def variableCase(context: Any, attributes: Any, name: Name): Int = 1
    }

    object ToString extends Folder[Any, Any, String] {
      def extensibleRecordCase(context: Any, attributes: Any, name: Name, fields: Chunk[Field[String]]): String =
        s"ExtensibleRecord(${name}, ${fields.map(_.data).mkString(", ")})"
      def functionCase(context: Any, attributes: Any, argumentType: String, returnType: String): String =
        s"Function(${argumentType}, ${returnType})"
      def recordCase(context: Any, attributes: Any, fields: Chunk[Field[String]]): String =
        s"Record(${fields.map(_.data).mkString(", ")})"
      def referenceCase(context: Any, attributes: Any, typeName: FQName, typeParams: Chunk[String]): String =
        s"Reference(${typeName}, ${typeParams.mkString(", ")})"
      def tupleCase(context: Any, attributes: Any, elements: Chunk[String]): String =
        elements.mkString("(", ", ", ")")
      def unitCase(context: Any, attributes: Any): String                 = "()"
      def variableCase(context: Any, attributes: Any, name: Name): String = name.toCamelCase
    }
  }
}

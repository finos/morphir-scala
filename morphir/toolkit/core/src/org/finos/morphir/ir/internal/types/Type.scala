package org.finos.morphir
package ir
package internal
package types

import org.finos.morphir.ir.types.Field
import scala.annotation.tailrec
import zio.Chunk
import zio.prelude._

sealed trait Type[+A] extends Product with Serializable { self =>
  import Type.{Unit => UnitType, _}

  final def ??(doc: String): Documented[Type[A]] = Documented(doc, self)

  def attributes: A

  // def flatMap[A1](f: A => Type[A1]): Type[A1] =
  //   fold[Type[A1]](
  //     unitCase0 = a => f(a),
  //     variableCase0 = (a, name) => f(a),
  //   )(extensibleRecordCase0 = ???, functionCase0 = ???, recordCase0 = ???, referenceCase0 = ???, tupleCase0 = ???)

  def collectReferences: Set[FQName] = fold[Set[FQName]](
    unitCase0 = _ => Set.empty,
    variableCase0 = (_, _) => Set.empty
  )(
    extensibleRecordCase0 = (a, _, fields) => fields.map(_.data).flatten.toSet,
    functionCase0 = (a, argumentType, returnType) => argumentType ++ returnType,
    recordCase0 = (a, fields) => fields.map(_.data).flatten.toSet,
    referenceCase0 = (a, typeName, typeParams) => typeParams.flatten.toSet + typeName,
    tupleCase0 = (a, types) => types.flatten.toSet
  )

  def collectVariables: Set[Name] = fold[Set[Name]](
    unitCase0 = _ => Set.empty,
    variableCase0 = (_, name) => Set(name)
  )(
    extensibleRecordCase0 = (a, name, fields) => fields.map(_.data).flatten.toSet + name,
    functionCase0 = (a, argumentType, returnType) => argumentType ++ returnType,
    recordCase0 = (a, fields) => fields.map(_.data).flatten.toSet,
    referenceCase0 = (a, typeName, typeParams) => typeParams.flatten.toSet,
    tupleCase0 = (a, types) => types.flatten.toSet
  )

  /**
   * Erase the attributes from this type.
   */
  def eraseAttributes: UType = self.mapAttributes(_ => ())

  def fold[Z](
      unitCase0: A => Z,
      variableCase0: (A, Name) => Z
  )(
      extensibleRecordCase0: (A, Name, Chunk[Field[Z]]) => Z,
      functionCase0: (A, Z, Z) => Z,
      recordCase0: (A, Chunk[Field[Z]]) => Z,
      referenceCase0: (A, FQName, Chunk[Z]) => Z,
      tupleCase0: (A, Chunk[Z]) => Z
  ): Z = foldContext[Unit, A, Z](())(new Type.Folder[Unit, A, Z] {
    def unitCase(context: Unit, attributes: A): Z                 = unitCase0(attributes)
    def variableCase(context: Unit, attributes: A, name: Name): Z = variableCase0(attributes, name)
    def extensibleRecordCase(context: Unit, attributes: A, name: Name, fields: Chunk[Field[Z]]): Z =
      extensibleRecordCase0(attributes, name, fields)
    def functionCase(context: Unit, attributes: A, argumentType: Z, returnType: Z): Z =
      functionCase0(attributes, argumentType, returnType)
    def recordCase(context: Unit, attributes: A, fields: Chunk[Field[Z]]): Z = recordCase0(attributes, fields)
    def referenceCase(context: Unit, attributes: A, typeName: FQName, typeParams: Chunk[Z]): Z =
      referenceCase0(attributes, typeName, typeParams)
    def tupleCase(context: Unit, attributes: A, elements: Chunk[Z]): Z = tupleCase0(attributes, elements)
  })

  final def foldContext[C, A1 >: A, Z](context: C)(folder: Folder[C, A1, Z]): Z = {
    import folder._
    sealed trait TypeCase {
      def attributes: A1
    }
    case class ExtensibleRecordCase(attributes: A1, name: Name, untypedFields: Chunk[Field.Untyped]) extends TypeCase
    case class FunctionCase(attributes: A1)                                                          extends TypeCase
    case class RecordCase(attributes: A1, untypedFields: Chunk[Field.Untyped])                       extends TypeCase
    case class ReferenceCase(attributes: A1, typeName: FQName, typeParamsSize: Int)                  extends TypeCase
    case class TupleCase(attributes: A1, arity: Int)                                                 extends TypeCase

    @tailrec
    def loop(in: List[Type[A]], out: List[Either[TypeCase, Z]]): List[Z] =
      in match {
        case ExtensibleRecord(attributes, name, fields) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          val untypedFields  = fields.map(f => Field.Untyped(f.name))
          loop(fieldTypeExprs ++ types, Left(ExtensibleRecordCase(attributes, name, untypedFields)) :: out)
        case Function(attributes, argumentType, returnType) :: types =>
          loop(argumentType :: returnType :: types, Left(FunctionCase(attributes)) :: out)
        case Record(attributes, fields) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          val untypedFields  = fields.map(f => Field.Untyped(f.name))
          loop(fieldTypeExprs ++ types, Left(RecordCase(attributes, untypedFields)) :: out)
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
            case (acc, Left(ExtensibleRecordCase(attributes, name, untypedFields))) =>
              val fieldTypes = acc.take(untypedFields.size)
              val rest       = acc.drop(untypedFields.size)
              val fields = untypedFields.zip(fieldTypes).map { case (field, fieldType) => Field(field.name, fieldType) }
              extensibleRecordCase(context, attributes, name, fields) :: rest
            case (acc, Left(FunctionCase(attributes))) =>
              val argumentType :: returnType :: rest = (acc: @unchecked)
              functionCase(context, attributes, argumentType, returnType) :: rest
            case (acc, Left(RecordCase(attributes, untypedFields))) =>
              val fieldTypes = acc.take(untypedFields.size)
              val rest       = acc.drop(untypedFields.size)
              val fields = untypedFields.zip(fieldTypes).map { case (field, fieldType) => Field(field.name, fieldType) }
              recordCase(context, attributes, fields) :: rest
            case (acc, Left(ReferenceCase(attributes, typeName, size))) =>
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

  final def foldLeft[Z](z: Z)(f: PartialFunction[(Z, Type[A]), Z]): Z = {
    @tailrec
    def loop(z: Z, typ: Type[A], stack: List[Type[A]]): Z =
      (f.applyOrElse[(Z, Type[A]), Z](z -> typ, _ => z), typ) match {
        case (z, ExtensibleRecord(_, _, Chunk(head, tail @ _*))) =>
          val rest = tail.map(_.data).toList
          loop(z, head.data, rest ++ stack)
        case (z, Function(_, argumentType, returnType)) =>
          loop(z, argumentType, returnType :: stack)
        case (z, Record(_, Chunk(head, tail @ _*))) =>
          val rest = tail.map(_.data).toList
          loop(z, head.data, rest ++ stack)
        case (z, Reference(_, _, Chunk(head, tail @ _*))) =>
          loop(z, head, tail.toList ++ stack)
        case (z, Tuple(_, Chunk(head, tail @ _*))) =>
          loop(z, head, tail.toList ++ stack)
        case (z, _) =>
          stack match {
            case head :: tail => loop(z, head, tail)
            case Nil          => z
          }
      }
    loop(z, self, Nil)
  }

  final def map[B](f: A => B): Type[B] =
    fold[Type[B]](
      attributes => UnitType(f(attributes)),
      (attributes, name) => Variable(f(attributes), name)
    )(
      (attributes, name, fields) => ExtensibleRecord(f(attributes), name, fields),
      (attributes, argumentType, returnType) => Function(f(attributes), argumentType, returnType),
      (attributes, fields) => Record(f(attributes), fields),
      (attributes, typeName, typeParams) => Reference(f(attributes), typeName, typeParams),
      (attributes, elements) => Tuple(f(attributes), elements)
    )

  /**
   * An alias for map.
   */
  @inline final def mapAttributes[B](f: A => B): Type[B] = map(f)

  final def satisfies(check: PartialFunction[Type[A], Boolean]): Boolean =
    check.lift(self).getOrElse(false)

  final def size: Int = foldContext(())(Type.Folder.Size)

  final override def toString: String = foldContext(())(Type.Folder.ToString)
}
object Type extends TypeConstructors with UnattributedTypeConstructors with FieldSyntax {
  type FieldT[+A] = Field[Type[A]]
  val FieldT: Field.type = Field

  type UType = Type[Any]
  val UType = Type

  def mapTypeAttributes[A](tpe: Type[A]): MapTypeAttributes[A] = new MapTypeAttributes(() => tpe)

  final case class ExtensibleRecord[+A](attributes: A, name: Name, fields: Chunk[Field[Type[A]]]) extends Type[A]
  object ExtensibleRecord {
    def apply[A](attributes: A, name: Name, fields: FieldT[A]*): ExtensibleRecord[A] =
      ExtensibleRecord(attributes, name, Chunk.fromIterable(fields))

    def apply[A](attributes: A, name: String, fields: FieldT[A]*): ExtensibleRecord[A] =
      ExtensibleRecord(attributes, Name.fromString(name), Chunk.fromIterable(fields))

    def apply(name: Name, fields: Field[UType]*): ExtensibleRecord[Any] =
      ExtensibleRecord((), name, Chunk.fromIterable(fields))

    def apply(name: Name, fields: Chunk[Field[UType]]): ExtensibleRecord[Any] =
      ExtensibleRecord((), name, fields)

    def apply(name: String, fields: Field[UType]*): ExtensibleRecord[Any] =
      ExtensibleRecord((), Name.fromString(name), Chunk.fromIterable(fields))

    def apply(name: String, fields: Chunk[Field[UType]]): ExtensibleRecord[Any] =
      ExtensibleRecord((), Name.fromString(name), fields)
  }
  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A]) extends Type[A]
  object Function {
    def apply(argumentType: UType, returnType: UType): Function[Any] = Function((), argumentType, returnType)
  }

  final case class Record[+A](attributes: A, fields: Chunk[Field[Type[A]]])                   extends Type[A]
  final case class Reference[+A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]]) extends Type[A]
  object Reference {
    def apply[A](attributes: A, typeName: FQName, typeParams: Type[A]*)(implicit ev: NeedsAttributes[A]): Reference[A] =
      Reference(attributes, typeName, Chunk.fromIterable(typeParams))

    def apply(typeName: FQName, typeParams: UType*): Reference[Any] =
      Reference((), typeName, Chunk.fromIterable(typeParams))
  }
  final case class Tuple[+A](attributes: A, elements: Chunk[Type[A]]) extends Type[A]
  object Tuple {
    def apply[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Tuple[A] =
      Tuple(attributes, Chunk.fromIterable(elements))

    def apply(elements: UType*): Tuple[Any] =
      Tuple((), Chunk.fromIterable(elements))
  }
  final case class Unit[+A](attributes: A)                 extends Type[A]
  final case class Variable[+A](attributes: A, name: Name) extends Type[A]
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
      def extensibleRecordCase(context: Any, attributes: Any, name: Name, fields: Chunk[Field[String]]): String = {
        val fieldList = fields.map(field => field.name.toCamelCase + " : " + field.data).mkString(", ")
        s"{ ${name.toCamelCase} | $fieldList }"
      }
      def functionCase(context: Any, attributes: Any, argumentType: String, returnType: String): String =
        s"$argumentType -> $returnType"
      def recordCase(context: Any, attributes: Any, fields: Chunk[Field[String]]): String =
        fields.map(field => field.name.toCamelCase + " : " + field.data).mkString("{ ", ", ", " }")
      def referenceCase(context: Any, attributes: Any, typeName: FQName, typeParams: Chunk[String]): String =
        typeName.toReferenceName + " " + typeParams.mkString(" ")
      def tupleCase(context: Any, attributes: Any, elements: Chunk[String]): String =
        elements.mkString("(", ", ", ")")
      def unitCase(context: Any, attributes: Any): String                 = "()"
      def variableCase(context: Any, attributes: Any, name: Name): String = name.toCamelCase
    }
  }

  implicit val CovariantType: Covariant[Type] = new Covariant[Type] {
    override def map[A, B](f: A => B): Type[A] => Type[B] = tpe => tpe.mapAttributes(f)
  }

  final class MapTypeAttributes[+A](val input: () => Type[A]) extends AnyVal {
    def apply[B](f: A => B): Type[B] = input().map(f)
  }

  implicit class UTypeExtensions(private val self: UType) extends AnyVal {
    def -->(that: UType): UType = Function(self, that)
  }
}

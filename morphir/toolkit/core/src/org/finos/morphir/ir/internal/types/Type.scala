package org.finos.morphir
package ir
package internal
package types

import io.TypeWriter
import scala.annotation.tailrec
import zio._
import zio.prelude._

private[internal] sealed trait Type[+A] extends Product with Serializable { self =>
  import Type.{Unit => UnitType, _}

  final def ??(doc: String): Documented[Type[A]] = Documented(doc, self)

  def attributes: A

  def exists(f: PartialFunction[Type[A], Any]): Boolean = find(f).isDefined

  def fieldCount: Int = foldLeft[Int](0) {
    case (acc, Record(_, fields))              => acc + fields.size
    case (acc, ExtensibleRecord(_, _, fields)) => acc + fields.size
    case (acc, _)                              => acc
  }

  def find[Z](f: PartialFunction[Type[A], Z]): Option[Z] = {
    @tailrec
    def loop(typ: Type[A], stack: List[Type[A]]): Option[Z] =
      f.lift(typ) match {
        case Some(z) => Some(z)
        case None =>
          typ match {
            case ExtensibleRecord(_, _, Chunk(head, tail @ _*)) =>
              val next = head.fieldType
              val rest = tail.map(_.fieldType) ++: stack
              loop(next, rest)
            case Function(_, argumentType, resultType) =>
              loop(argumentType, resultType :: stack)
            case Record(_, Chunk(head, tail @ _*)) =>
              val next = head.fieldType
              val rest = tail.map(_.fieldType) ++: stack
              loop(next, rest)
            case Reference(_, _, Chunk(head, tail @ _*)) =>
              loop(head, tail ++: stack)
            case Tuple(_, Chunk(head, tail @ _*)) =>
              loop(head, tail ++: stack)
            case _ =>
              stack match {
                case head :: tail => loop(head, tail)
                case Nil          => None
              }
          }
      }

    loop(self, Nil)
  }

  def findAll[Z](f: PartialFunction[Type[A], Z]): List[Z] = foldLeft[List[Z]](Nil) {
    case (acc, typ) if f.isDefinedAt(typ) => f(typ) :: acc
    case (acc, _)                         => acc
  }

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
    def unitCase(context: Unit, tpe: Type[A], attributes: A): Z                 = unitCase0(attributes)
    def variableCase(context: Unit, tpe: Type[A], attributes: A, name: Name): Z = variableCase0(attributes, name)
    def extensibleRecordCase(context: Unit, tpe: Type[A], attributes: A, name: Name, fields: Chunk[Field[Z]]): Z =
      extensibleRecordCase0(attributes, name, fields)
    def functionCase(context: Unit, tpe: Type[A], attributes: A, argumentType: Z, returnType: Z): Z =
      functionCase0(attributes, argumentType, returnType)
    def recordCase(context: Unit, tpe: Type[A], attributes: A, fields: Chunk[Field[Z]]): Z =
      recordCase0(attributes, fields)
    def referenceCase(context: Unit, tpe: Type[A], attributes: A, typeName: FQName, typeParams: Chunk[Z]): Z =
      referenceCase0(attributes, typeName, typeParams)
    def tupleCase(context: Unit, tpe: Type[A], attributes: A, elements: Chunk[Z]): Z = tupleCase0(attributes, elements)
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
    def loop(in: List[Type[A]], out: List[Either[Type[A], Z]]): List[Z] =
      in match {
        case (t @ ExtensibleRecord(attributes, name, fields)) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          val untypedFields  = fields.map(f => Field.Untyped(f.name))
          loop(fieldTypeExprs ++ types, Left(t) :: out)
        case (t @ Function(attributes, argumentType, returnType)) :: types =>
          loop(argumentType :: returnType :: types, Left(t) :: out)
        case (t @ Record(attributes, fields)) :: types =>
          val fieldTypeExprs = fields.map(_.data).toList
          val untypedFields  = fields.map(f => Field.Untyped(f.name))
          loop(fieldTypeExprs ++ types, Left(t) :: out)
        case (t @ Reference(attributes, typeName, typeParams)) :: types =>
          loop(typeParams.toList ++ types, Left(t) :: out)
        case (t @ Tuple(attributes, elements)) :: types =>
          loop(elements.toList ++ types, Left(t) :: out)
        case (t @ UnitType(attributes)) :: types => loop(types, Right(unitCase(context, t, attributes)) :: out)
        case (t @ Variable(attributes, name)) :: types =>
          loop(types, Right(variableCase(context, t, attributes, name)) :: out)
        case Nil =>
          out.foldLeft[List[Z]](List.empty) {
            case (acc, Right(results)) => results :: acc
            case (acc, Left(t @ ExtensibleRecord(attributes, name, _))) =>
              val size       = t.fields.size
              val fieldTypes = acc.take(size)
              val rest       = acc.drop(size)
              val fields     = t.fields.zip(fieldTypes).map { case (field, fieldType) => Field(field.name, fieldType) }
              extensibleRecordCase(context, t, attributes, name, fields) :: rest
            case (acc, Left(t @ Function(attributes, _, _))) =>
              val argumentType :: returnType :: rest = (acc: @unchecked)
              functionCase(context, t, attributes, argumentType, returnType) :: rest
            case (acc, Left(t @ Record(attributes, _))) =>
              val size       = t.fields.size
              val fieldTypes = acc.take(size)
              val rest       = acc.drop(size)
              val fields     = t.fields.zip(fieldTypes).map { case (field, fieldType) => Field(field.name, fieldType) }
              recordCase(context, t, attributes, fields) :: rest
            case (acc, Left(t @ Reference(attributes, typeName, _))) =>
              val size       = t.typeParams.size
              val typeParams = Chunk.fromIterable(acc.take(size))
              val rest       = acc.drop(size)
              referenceCase(context, t, attributes, typeName, typeParams) :: rest
            case (acc, Left(t @ Tuple(attributes, _))) =>
              val arity    = t.elements.size
              val elements = Chunk.fromIterable(acc.take(arity))
              val rest     = acc.drop(arity)
              tupleCase(context, t, attributes, elements) :: rest
            case (acc, Left(t)) =>
              throw new IllegalStateException(
                s"Unexpected type ${t.getClass.getSimpleName()} encountered during transformation. (Type Expr: $t)"
              )
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

  /**
   * Uses the specified function to map the FQName of any type references to another FQName.
   */
  final def mapReferenceName(f: FQName => FQName): Type[A] = foldContext(())(Folder.MapReferenceName(f))

  final def satisfies(check: PartialFunction[Type[A], Boolean]): Boolean =
    check.lift(self).getOrElse(false)

  final def size: Int = foldContext(())(Type.Folder.Size)

  final override def toString: String = foldContext((): Any)(Type.Folder.ToString)

  def write[Context](context: Context)(writer: TypeWriter[Context, A]): Unit =
    self match {
      case ExtensibleRecord(attributes, name, fields)     => ???
      case Function(attributes, argumentType, returnType) => ???
      case Record(attributes, fields)                     => ???
      case Reference(attributes, typeName, typeParams)    => ???
      case Tuple(attributes, elements)                    => ???
      case UnitType(attributes)                           => writer.writeUnit(context, attributes)
      case Variable(attributes, name)                     => writer.writeVariable(context, attributes, name)
    }

  def writeZIO[Context: Tag](writer: TypeWriter[Context, A]): ZIO[Context, Throwable, Unit] =
    self match {
      case ExtensibleRecord(attributes, name, fields)     => ???
      case Function(attributes, argumentType, returnType) => ???
      case Record(attributes, fields)                     => ???
      case Reference(attributes, typeName, typeParams)    => ???
      case Tuple(attributes, elements)                    => ???
      case UnitType(attributes)                           => writer.writeUnitZIO(attributes)
      case Variable(attributes, name)                     => writer.writeVariableZIO(attributes, name)
    }
}
private[internal] object Type extends TypeConstructors with UnattributedTypeConstructors with FieldSyntax {
  type FieldT[+A] = Field[Type[A]]
  val FieldT: Field.type = Field

  type UType = Type[Any]
  val UType = Type

  type UExtensibleRecord = ExtensibleRecord[Any]
  val UExtensibleRecord = ExtensibleRecord

  type UFunction = Function[Any]
  val UFunction = Function

  type URecord = Record[Any]
  val URecord = Record

  type UReference = Reference[Any]
  val UReference = Reference

  type UTuple = Tuple[Any]
  val UTuple = Tuple

  type UUnit = Type.Unit[Any]
  val UUnit = Type.Unit

  type UVariable = Variable[Any]
  val UVariable = Variable

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

  final case class Record[+A](attributes: A, fields: Chunk[Field[Type[A]]]) extends Type[A]
  object Record {
    val empty: Record[Any]                              = Record((), Chunk.empty)
    def apply(fields: Chunk[Field[UType]]): Record[Any] = Record((), fields)

    def apply(fields: FieldT[Any]*): Record[Any] = Record((), Chunk.fromIterable(fields))

    def empty[A](attributes: A): Record[A] = Record(attributes, Chunk.empty)
    final class CreateAttributed[A](val attributes: A) extends AnyVal {
      def apply(fields: Chunk[FieldT[A]]): Type[A] = Record(attributes, fields)
      def apply(fields: FieldT[A]*): UType         = Record(attributes, Chunk.fromIterable(fields))
    }
  }
  final case class Reference[+A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]]) extends Type[A]
  object Reference {
    def apply[A](attributes: A, typeName: FQName, typeParams: Type[A]*)(implicit ev: NeedsAttributes[A]): Reference[A] =
      Reference(attributes, typeName, Chunk.fromIterable(typeParams))

    def apply(typeName: FQName, typeParams: UType*): UReference =
      Reference((), typeName, Chunk.fromIterable(typeParams))

    def apply(name: FQName): ApplyGivenName = new ApplyGivenName(name)
    def apply(name: String): ApplyGivenName = new ApplyGivenName(FQName.fromString(name))

    final class ApplyGivenName(val name: FQName) extends AnyVal {
      def apply(typeParams: UType*): UReference = Reference((), name, Chunk.fromIterable(typeParams))
    }
  }
  final case class Tuple[+A](attributes: A, elements: Chunk[Type[A]]) extends Type[A]
  object Tuple {
    def apply[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Tuple[A] =
      Tuple(attributes, Chunk.fromIterable(elements))

    def apply(elements: UType*): UTuple =
      Tuple((), Chunk.fromIterable(elements))

    def withElements(elements: UType*): UTuple = Tuple((), Chunk.fromIterable(elements))
  }
  final case class Unit[+A](attributes: A)                 extends Type[A]
  final case class Variable[+A](attributes: A, name: Name) extends Type[A]
  object Variable {
    def apply(name: String): Variable[Any]             = Variable((), Name.fromString(name))
    def apply(name: Name): Variable[Any]               = Variable((), name)
    def apply[A](attributes: A, name: String): Type[A] = Variable(attributes, Name.fromString(name))
  }

  trait Folder[-Context, -Attrib, Z] {
    def extensibleRecordCase(
        context: Context,
        tpe: Type[Attrib],
        attributes: Attrib,
        name: Name,
        fields: Chunk[Field[Z]]
    ): Z
    def functionCase(context: Context, tpe: Type[Attrib], attributes: Attrib, argumentType: Z, returnType: Z): Z
    def recordCase(context: Context, tpe: Type[Attrib], attributes: Attrib, fields: Chunk[Field[Z]]): Z
    def referenceCase(
        context: Context,
        tpe: Type[Attrib],
        attributes: Attrib,
        typeName: FQName,
        typeParams: Chunk[Z]
    ): Z
    def tupleCase(context: Context, tpe: Type[Attrib], attributes: Attrib, elements: Chunk[Z]): Z
    def unitCase(context: Context, tpe: Type[Attrib], attributes: Attrib): Z
    def variableCase(context: Context, tpe: Type[Attrib], attributes: Attrib, name: Name): Z
  }

  object Folder {
    abstract class RewritingFolder[-Context, Attrib] extends Folder[Context, Attrib, Type[Attrib]] {
      def extensibleRecordCase(
          context: Context,
          tpe: Type[Attrib],
          attributes: Attrib,
          name: Name,
          fields: Chunk[Field[Type[Attrib]]]
      ): Type[Attrib] =
        ExtensibleRecord(attributes, name, fields)
      def functionCase(
          context: Context,
          tpe: Type[Attrib],
          attributes: Attrib,
          argumentType: Type[Attrib],
          returnType: Type[Attrib]
      ): Type[Attrib] =
        Function(attributes, argumentType, returnType)
      def recordCase(
          context: Context,
          tpe: Type[Attrib],
          attributes: Attrib,
          fields: Chunk[Field[Type[Attrib]]]
      ): Type[Attrib] =
        Record(attributes, fields)
      def referenceCase(
          context: Context,
          tpe: Type[Attrib],
          attributes: Attrib,
          typeName: FQName,
          typeParams: Chunk[Type[Attrib]]
      ): Type[Attrib] =
        Reference(attributes, typeName, typeParams)
      def tupleCase(
          context: Context,
          tpe: Type[Attrib],
          attributes: Attrib,
          elements: Chunk[Type[Attrib]]
      ): Type[Attrib] =
        Tuple(attributes, elements)
      def unitCase(context: Context, tpe: Type[Attrib], attributes: Attrib): Type[Attrib] = Unit(attributes)
      def variableCase(context: Context, tpe: Type[Attrib], attributes: Attrib, name: Name): Type[Attrib] =
        Variable(attributes, name)
    }

    object Size extends Folder[Any, Any, Int] {
      def extensibleRecordCase(
          context: Any,
          tpe: Type[Any],
          attributes: Any,
          name: Name,
          fields: Chunk[Field[Int]]
      ): Int =
        fields.map(_.data).sum + 1
      def functionCase(context: Any, tpe: Type[Any], attributes: Any, argumentType: Int, returnType: Int): Int =
        argumentType + returnType + 1
      def recordCase(context: Any, tpe: Type[Any], attributes: Any, fields: Chunk[Field[Int]]): Int =
        fields.map(_.data).sum + 1
      def referenceCase(
          context: Any,
          tpe: Type[Any],
          attributes: Any,
          typeName: FQName,
          typeParams: Chunk[Int]
      ): Int =
        typeParams.sum + 1
      def tupleCase(context: Any, tpe: Type[Any], attributes: Any, elements: Chunk[Int]): Int =
        elements.sum + 1
      def unitCase(context: Any, tpe: Type[Any], attributes: Any): Int                 = 1
      def variableCase(context: Any, tpe: Type[Any], attributes: Any, name: Name): Int = 1
    }

    object ToString extends Folder[Any, Any, String] {
      def extensibleRecordCase(
          context: Any,
          tpe: Type[Any],
          attributes: Any,
          name: Name,
          fields: Chunk[Field[String]]
      ): String = {
        val fieldList = fields.map(field => field.name.toCamelCase + " : " + field.data).mkString(", ")
        s"{ ${name.toCamelCase} | $fieldList }"
      }
      def functionCase(
          context: Any,
          tpe: Type[Any],
          attributes: Any,
          argumentType: String,
          returnType: String
      ): String =
        tpe match {
          case Function(_, argumentType: Function[Any], _) => s"($argumentType) -> $returnType"
          case _                                           => s"$argumentType -> $returnType"
        }
      def recordCase(context: Any, tpe: Type[Any], attributes: Any, fields: Chunk[Field[String]]): String =
        fields.map(field => field.name.toCamelCase + " : " + field.data).mkString("{ ", ", ", " }")
      def referenceCase(
          context: Any,
          tpe: Type[Any],
          attributes: Any,
          typeName: FQName,
          typeParams: Chunk[String]
      ): String =
        (typeName.toReferenceName +: typeParams).mkString(" ")
      def tupleCase(context: Any, tpe: Type[Any], attributes: Any, elements: Chunk[String]): String =
        elements.mkString("(", ", ", ")")
      def unitCase(context: Any, tpe: Type[Any], attributes: Any): String                 = "()"
      def variableCase(context: Any, tpe: Type[Any], attributes: Any, name: Name): String = name.toCamelCase
    }

    // typeX.mapReferenceCaseName(_.toUpperCase).mapVariableName(_.toLowerCase)

    final case class MapReferenceName[Attrib](f: FQName => FQName) extends RewritingFolder[Any, Attrib] {
      override def referenceCase(
          context: Any,
          tpe: Type[Attrib],
          attributes: Attrib,
          typeName: FQName,
          typeParams: Chunk[Type[Attrib]]
      ): Type[Attrib] =
        Reference(attributes, f(typeName), typeParams)

    }
  }

  abstract class ForEach[-Context, -Attrib] extends Folder[Context, Attrib, scala.Unit] {
    final def extensibleRecordCase(
        context: Context,
        attributes: Attrib,
        name: Name,
        fields: Chunk[Field[scala.Unit]]
    ): scala.Unit = onExtensibleRecord(context, attributes, name, fields.size)

    final def functionCase(
        context: Context,
        attributes: Attrib,
        argumentType: scala.Unit,
        returnType: scala.Unit
    ): scala.Unit =
      onFunction(context, attributes)

    def onFunction(context: Context, attributes: Attrib): scala.Unit                                        = ()
    def onExtensibleRecord(context: Context, attributes: Attrib, name: Name, fieldSize: Int): scala.Unit    = ()
    def onRecord(context: Context, attributes: Attrib, fieldSize: Int): scala.Unit                          = ()
    def onReference(context: Context, attributes: Attrib, typeName: FQName, typeParamSize: Int): scala.Unit = ()
    def onTuple(context: Context, attributes: Attrib, arity: Int): scala.Unit                               = ()
    def unit(context: Context, attributes: Attrib): scala.Unit                                              = ()
    def variable(context: Context, attributes: Attrib, name: Name): scala.Unit                              = ()

    final def recordCase(context: Context, attributes: Attrib, fields: Chunk[Field[scala.Unit]]): scala.Unit =
      onRecord(context, attributes, fields.size)
    def referenceCase(
        context: Context,
        attributes: Attrib,
        typeName: FQName,
        typeParams: Chunk[scala.Unit]
    ): scala.Unit = onReference(context, attributes, typeName, typeParams.size)

    final def tupleCase(context: Context, attributes: Attrib, elements: Chunk[scala.Unit]): scala.Unit =
      onTuple(context, attributes, elements.size)

    def unitCase(context: Context, attributes: Attrib): scala.Unit                 = unit(context, attributes)
    def variableCase(context: Context, attributes: Attrib, name: Name): scala.Unit = variable(context, attributes, name)
  }

  // TODO: Look at usecases
  trait ForEachZIO[-Context, -Attrib] extends Folder[Context, Attrib, Task[Any]] {
    
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

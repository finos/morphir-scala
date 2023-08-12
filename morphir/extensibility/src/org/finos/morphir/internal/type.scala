package org.finos.morphir.internal

import org.finos.morphir.functional._
import org.finos.morphir.naming._

import scala.annotation.tailrec
trait TypeModule extends TypeModuleVersionSpecific { self: DocumentedModule with TypeTransformerModule =>

  sealed trait TypeExpr { self =>
    def attributes: Any

  }
  sealed trait Type[+A] extends TypeExpr { self =>
    import Type.{Unit => UnitType, _}
    type AttributesType <: A

    def ??(doc: String): Documented[Type[A]] = Documented(doc, this)
    override def attributes: A

    final def exists(f: PartialFunction[Type[A], Any]): Boolean = find(f).isDefined

    final def find[Z](f: PartialFunction[Type[A], Z]): Option[Z] = {
      @tailrec
      def loop(typ: Type[A], stack: List[Type[A]]): Option[Z] =
        f.lift(typ) match {
          case Some(z) => Some(z)
          case None =>
            typ match {
              case ExtensibleRecord(_, _, head :: tail) =>
                val next = head.data
                val rest = tail.map(_.data) ++: stack
                loop(next, rest)
              case Function(_, argumentType, resultType) =>
                loop(argumentType, resultType :: stack)
              case Record(_, List(head, tail @ _*)) =>
                val next = head.data
                val rest = tail.map(_.data) ++: stack
                loop(next, rest)
              case Reference(_, _, List(head, tail @ _*)) =>
                loop(head, tail ++: stack)
              case Tuple(_, List(head, tail @ _*)) =>
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

    lazy val fieldCount: Int = foldLeftSome[Int](0) {
      case (acc, Record(_, fields))              => acc + fields.size
      case (acc, ExtensibleRecord(_, _, fields)) => acc + fields.size
      case (acc, _)                              => acc
    }

    final def foldLeft[Z](z: Z)(f: (Z, Type[A]) => Z): Z = {

      @tailrec
      def loop(remaining: List[Type[A]], acc: Z): Z = remaining match {
        case Nil => acc
        case head :: tail => head match {
            case e: ExtensibleRecord[A] =>
              loop(e.fields.map(_.data).view.toList ++ tail, f(acc, e))

            case fun: Function[A] =>
              loop(fun.argumentType :: fun.returnType :: tail, f(acc, fun))

            case rec: Record[A] =>
              loop(rec.fields.map(_.data).view.toList ++ tail, f(acc, rec))

            case ref: Reference[A] =>
              loop(ref.typeParams.view.toList ++ tail, f(acc, ref))

            case t: Tuple[A] =>
              loop(t.elements.view.toList ++ tail, f(acc, t))

            case u: UnitType[A] =>
              loop(tail, f(acc, u))

            case v: Variable[A] =>
              loop(tail, f(acc, v))
          }
      }

      loop(List(this), z)
    }

    final def foldLeftSome[Z](z: Z)(f: PartialFunction[(Z, Type[A]), Z]): Z = {

      @tailrec
      def loop(remaining: List[Type[A]], acc: Z): Z = remaining match {
        case Nil => acc
        case head :: tail =>
          val newAcc = if (f.isDefinedAt((acc, head))) f((acc, head)) else acc
          head match {
            case ExtensibleRecord(_, _, fields) =>
              loop(fields.map(_.data) ++ tail, newAcc)

            case Function(_, argumentType, returnType) =>
              loop(argumentType :: returnType :: tail, newAcc)

            case Record(_, fields) =>
              loop(fields.map(_.data) ++ tail, newAcc)

            case Reference(_, _, typeParams) =>
              loop(typeParams ++ tail, newAcc)

            case Tuple(_, elements) =>
              loop(elements ++ tail, newAcc)

            // The following
            case UnitType(_) =>
              loop(tail, newAcc)

            case Variable(_, _) =>
              loop(tail, newAcc)
          }
      }

      loop(List(this), z)
    }

    def foldUp[Z](z: Z)(f: (Type[A], List[Z]) => Z): Z = {
      def loop(node: Type[A], childrenResults: List[Z] = Nil): Z = node match {
        case e: ExtensibleRecord[A] =>
          val childrenValues = e.fields.map(field => loop(field.data))
          f(e, childrenValues)

        case fun: Function[A] =>
          val argValue = loop(fun.argumentType)
          val retValue = loop(fun.returnType)
          f(fun, List(argValue, retValue))

        case rec: Record[A] =>
          val childrenValues = rec.fields.map(field => loop(field.data))
          f(rec, childrenValues)

        case ref: Reference[A] =>
          val typeParamValues = ref.typeParams.map(tp => loop(tp))
          f(ref, typeParamValues)

        case t: Tuple[A] =>
          val elementValues = t.elements.map(e => loop(e))
          f(t, elementValues)

        case u: UnitType[A] =>
          f(u, Nil)

        case v: Variable[A] =>
          f(v, Nil)
      }

      loop(this)
    }

    def fold[Z](
        unitCase0: A => Z,
        variableCase0: (A, Name) => Z,
        extensibleRecordCase0: (A, Name, List[IField[Z]]) => Z,
        functionCase0: (A, Z, Z) => Z,
        recordCase0: (A, List[IField[Z]]) => Z,
        referenceCase0: (A, FQName, List[Z]) => Z,
        tupleCase0: (A, List[Z]) => Z
    ): Z = TypeFolder.foldContext[Any, A, Z](self, ())(
      new TypeFolder[Any, A, Z] {
        def unitCase(context: Any, tpe: Type[A], attributes: A): Z = unitCase0(attributes)

        def variableCase(context: Any, tpe: Type[A], attributes: A, name: Name): Z = variableCase0(attributes, name)

        def extensibleRecordCase(context: Any, tpe: Type[A], attributes: A, name: Name, fields: List[IField[Z]]): Z =
          extensibleRecordCase0(attributes, name, fields)

        def functionCase(context: Any, tpe: Type[A], attributes: A, argumentType: Z, returnType: Z): Z =
          functionCase0(attributes, argumentType, returnType)

        def recordCase(context: Any, tpe: Type[A], attributes: A, fields: List[IField[Z]]): Z =
          recordCase0(attributes, fields)

        def referenceCase(context: Any, tpe: Type[A], attributes: A, typeName: FQName, typeParams: List[Z]): Z =
          referenceCase0(attributes, typeName, typeParams)

        def tupleCase(context: Any, tpe: Type[A], attributes: A, elements: List[Z]): Z =
          tupleCase0(attributes, elements)
      }
    )

    def mapAttributes[B](f: A => B): Type[B] = {
      sealed trait ProcessTask
      case class Process[T >: Type[A]](node: T, processedChildren: List[Type[B]]) extends ProcessTask

      @scala.annotation.tailrec
      def loop(stack: List[ProcessTask], accumulator: List[Type[B]]): Type[B] = stack match {
        case Nil => accumulator.head
        case Process(node, children) :: tail => node match {
            case e @ ExtensibleRecord(_, _, _) if children.length < e.fields.length =>
              loop(Process(e.fields(children.length).data, Nil) :: stack, accumulator)
            case e @ ExtensibleRecord(_, _, _) =>
              val newFields = children.takeRight(e.fields.length).map(t => Field(t.asInstanceOf[Field[B]].name, t))
              loop(tail, ExtensibleRecord(f(e.attributes.asInstanceOf[A]), e.name, newFields.reverse) :: accumulator)

            case fun @ Function(_, _, _) if children.isEmpty =>
              loop(Process(fun.argumentType, Nil) :: stack, accumulator)
            case fun @ Function(_, _, _) if children.length == 1 =>
              loop(Process(fun.returnType, Nil) :: stack, accumulator)
            case fun @ Function(_, _, _) =>
              loop(tail, Function(f(fun.attributes.asInstanceOf[A]), children(1), children.head) :: accumulator)

            case rec @ Record(_, _) if children.length < rec.fields.length =>
              loop(Process(rec.fields(children.length).data, Nil) :: stack, accumulator)
            case rec @ Record(_, _) =>
              val newFields = children.takeRight(rec.fields.length).map(t => Field(t.asInstanceOf[Field[B]].name, t))
              loop(tail, Record(f(rec.attributes.asInstanceOf[A]), newFields.reverse) :: accumulator)

            case ref @ Reference(_, _, _) if children.length < ref.typeParams.length =>
              loop(Process(ref.typeParams(children.length), Nil) :: stack, accumulator)
            case ref @ Reference(_, _, _) =>
              loop(tail, Reference(f(ref.attributes.asInstanceOf[A]), ref.typeName, children.reverse) :: accumulator)

            case t @ Tuple(_, _) if children.length < t.elements.length =>
              loop(Process(t.elements(children.length), Nil) :: stack, accumulator)
            case t @ Tuple(_, _) =>
              loop(tail, Tuple(f(t.attributes.asInstanceOf[A]), children.reverse) :: accumulator)

            case u @ UnitType(_) =>
              loop(tail, UnitType(f(u.attributes.asInstanceOf[A])) :: accumulator)

            case v @ Variable(_, _) =>
              loop(tail, Variable(f(v.attributes.asInstanceOf[A]), v.name) :: accumulator)
          }
      }

      loop(List(Process(this, Nil)), Nil)
    }

    final def map[B](f: A => B): Type[B] =
      fold[Type[B]](
        attributes => UnitType(f(attributes)),
        (attributes, name) => Variable(f(attributes), name),
        (attributes, name, fields) => ExtensibleRecord(f(attributes), name, fields.asInstanceOf[List[Field[B]]]),
        (attributes, argumentType, returnType) => Function(f(attributes), argumentType, returnType),
        (attributes, fields) => Record(f(attributes), fields.asInstanceOf[List[Field[B]]]),
        (attributes, typeName, typeParams) => Reference(f(attributes), typeName, typeParams),
        (attributes, elements) => Tuple(f(attributes), elements)
      )

    lazy val size: Long = foldLeft(0L) {
      case (acc, _) => acc + 1
    }

//    def transform[B](f: Type[A] => Type[B]): Type[B] = {
//
//      // Explicit stack to avoid blowing up the call stack
//      var stack: List[(Type[A], List[Type[B]])] = List((this, Nil))
//      var result: Option[Type[B]]               = None
//
//      @scala.annotation.tailrec
//      def processStack(): scala.Unit = stack match {
//        case Nil => ()
//
//        case (current, siblings) :: rest =>
//          current match {
//            case e: ExtensibleRecord[A] =>
//              val transformedFields = e.fields.map(field => field.copy(data = field.data.transform(f)))
//              val newNode: Type[B]  = f(ExtensibleRecord(e.attributes, e.name, transformedFields))
//              if (transformedFields.isEmpty) {
//                stack = rest
//                result = Some(newNode)
//              } else {
//                stack = (transformedFields.head.data, transformedFields.tail.map(_.data)) :: stack
//              }
//
//            case fun: Function[A] =>
//              val argType          = fun.argumentType.transform(f)
//              val returnType       = fun.returnType.transform(f)
//              val newNode: Type[B] = f(Function(fun.attributes, argType, returnType))
//              stack = rest
//              result = Some(newNode)
//
//            case rec: Record[A] =>
//              val transformedFields = rec.fields.map(field => field.copy(data = field.data.transform(f)))
//              val newNode: Type[B]  = f(Record(rec.attributes, transformedFields))
//              if (transformedFields.isEmpty) {
//                stack = rest
//                result = Some(newNode)
//              } else {
//                stack = (transformedFields.head.data, transformedFields.tail.map(_.data)) :: stack
//              }
//
//            case ref: Reference[A] =>
//              val typeParams       = ref.typeParams.map(_.transform(f))
//              val newNode: Type[B] = f(Reference(ref.attributes, ref.typeName, typeParams))
//              stack = rest
//              result = Some(newNode)
//
//            case t: Tuple[A] =>
//              val elements         = t.elements.map(_.transform(f))
//              val newNode: Type[B] = f(Tuple(t.attributes, elements))
//              stack = rest
//              result = Some(newNode)
//
//            case u: Unit[A] =>
//              val newNode: Type[B] = f(u)
//              stack = rest
//              result = Some(newNode)
//
//            case v: Variable[A] =>
//              val newNode: Type[B] = f(v)
//              stack = rest
//              result = Some(newNode)
//          }
//      }
//
//      processStack()
//      result.getOrElse(throw new RuntimeException("Transformation failed. This shouldn't happen."))
//    }

  }

  object Type {

    def reference[A](attributes: A)(name: FQName, typeParams: List[Type[A]] = List.empty): Reference[A] =
      Reference(attributes, name, typeParams)

    sealed case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]]) extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A]) extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[A]])                       extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]])
        extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]]) extends Type[A]
    sealed case class Unit[+A](attributes: A)                           extends Type[A]
    sealed case class Variable[+A](attributes: A, name: Name)           extends Type[A]

    implicit val CovariantTypeInstance: Covariant[Type] = new Covariant[Type] {
      override def map[A, B](fa: Type[A])(f: A => B): Type[B] = fa.map(f)
    }
  }

  type Field[+A] = FieldK[Type, A]
  object Field {
    def apply[A](name: String, tpe: Type[A]): Field[A] = FieldK(Name.fromString(name), tpe)
    def apply[A](name: Name, tpe: Type[A]): Field[A]   = FieldK(name, tpe)

    type Untyped = Field[Unit]

    object Untyped {
      // def apply(name: Name): Field[Unit] = Field(name, ())
      def unapply(field: Field[Unit]): Name = field.name
    }
  }

  sealed case class FieldK[F[+_], +A](name: Name, data: F[A]) {
    @inline def tpe[A0 >: A](implicit ev: F[A] <:< Type[A0]): F[A0] = data
    def map[B](f: A => B)(implicit covariant: Covariant[F]): FieldK[F, B] =
      FieldK(name, covariant.map(data)(f))
  }
  object FieldK {
    def apply[F[+_], A](name: String, data: F[A]): FieldK[F, A] = FieldK(Name.fromString(name), data)
  }

  type IField[+A] = FieldK[Id, A]
  object IField {
    def apply[A](name: String, data: A): IField[A] = FieldK(Name.fromString(name), Id[A](data))
    def apply[A](name: Name, data: A): IField[A]   = FieldK(name, Id[A](data))
  }

}

package org.finos.morphir.internal

import org.finos.morphir.functional._
import org.finos.morphir.naming._

import scala.annotation.tailrec
trait TypeModule extends TypeModuleVersionSpecific { self: DocumentedModule =>

  sealed trait Type[+A] { self =>
    import Type.*

    def ??(doc: String): Documented[Type[A]] = Documented(doc, this)
    def attributes: A

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

    def fieldCount: Int = foldLeftSome[Int](0) {
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

            case u: Unit[A] =>
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
            case Unit(_) =>
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

        case u: Unit[A] =>
          f(u, Nil)

        case v: Variable[A] =>
          f(v, Nil)
      }

      loop(this)
    }

    def map[B](f: A => B): Type[B] = ???

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
  object FieldK {}

  type IField[+A] = FieldK[Id, A]
  object IField {
    def apply[A](name: String, data: A): IField[A] = FieldK(Name.fromString(name), Id[A](data))
    def apply[A](name: Name, data: A): IField[A]   = FieldK(name, Id[A](data))
  }

}

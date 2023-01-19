package org.finos
package morphir
package ir

import morphir.prelude.*
import FQName.FQName
import Name.Name

object Type {
  final case class Constructor[+A](name: Name, args: ConstructorArgs[A]) {
    def map[B](f: A => B): Constructor[B] = copy(args = args.map(f))
  }

  final case class Constructors[+A](byName: Map[Name, Constructor[A]]) {
    def map[B](f: A => B): Constructors[B]       = Constructors(byName.view.mapValues(_.map(f)).toMap)
    @inline def toMap: Map[Name, Constructor[A]] = byName
  }

  object Constructors {
    implicit def toMap[A](ctors: Constructors[A]): Map[Name, Constructor[A]] = ctors.byName
  }

  final case class ConstructorArg[+A](name: Name, tpe: Type[A]) {
    def map[B](f: A => B): ConstructorArg[B] = ConstructorArg(name, tpe.map(f))
  }
  object ConstructorArg {
    implicit def toTuple[A](arg: ConstructorArg[A]): (Name, Type[A]) = (arg.name, arg.tpe)
  }
  final case class ConstructorArgs[+A](args: List[ConstructorArg[A]]) extends AnyVal { self =>
    def map[B](f: A => B): ConstructorArgs[B] = ConstructorArgs(self.args.map(_.map(f)))
    def toList: List[ConstructorArg[A]]       = args
  }
  object ConstructorArgs {
    implicit def toList[A](args: ConstructorArgs[A]): List[ConstructorArg[A]] = args.args
  }

  sealed trait Type[+A] {
    def map[B](f: A => B): Type[B] = ???
  }
  object Type {

    sealed case class ExtensibleRecord[+A](attributes: A, name: Name, fields: List[Field[A]])   extends Type[A]
    sealed case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])   extends Type[A]
    sealed case class Record[+A](attributes: A, fields: List[Field[A]])                         extends Type[A]
    sealed case class Reference[+A](attributes: A, typeName: FQName, typeParams: List[Type[A]]) extends Type[A]
    sealed case class Tuple[+A](attributes: A, elements: List[Type[A]])                         extends Type[A]
    sealed case class Unit[+A](attributes: A)                                                   extends Type[A]
    sealed case class Variable[+A](attributes: A, name: Name)                                   extends Type[A]
  }

  final case class Field[+A](name: String, tpe: Type[A]) {
    def map[B](f: A => B): Field[B] = Field(name, tpe.map(f))
  }

  final case class Spec[Props[+_], +A](typeParams: List[Name], properties: Props[A])
  object Spec {
    import Specification.*
    type TypeAlias[+A] = Spec[Properties.TypeAlias, A]
    object TypeAlias {
      def apply[A](typeParams: List[Name], properties: Properties.TypeAlias[A]): TypeAlias[A] =
        Spec(typeParams, properties)
      def unapply[F[+_]](spec: Spec[F, _]): Option[(List[Name], Type[_])] = spec match {
        case Spec(typeParams, Properties.TypeAlias(typeExpr)) => Some((typeParams, typeExpr))
        case _                                                => None
      }
    }

    type OpaqueType = Spec[Properties, Nothing]
    object OpaqueType {
      def apply(typeParams: List[Name]): OpaqueType =
        Spec(typeParams, Properties.OpaqueType)
      def unapply[F[+_]](spec: Spec[F, _]): Option[List[Name]] = spec match {
        case Spec(typeParams, Properties.OpaqueType) => Some(typeParams)
        case _                                       => None
      }
    }

    type CustomType[+A] = Spec[Properties.CustomType, A]
    object CustomType {
      def apply[A](typeParams: List[Name], properties: Properties.CustomType[A]): CustomType[A] =
        Spec(typeParams, properties)
      def unapply[F[+_]](spec: Spec[F, _]): Option[(List[Name], Constructors[_])] = spec match {
        case Spec(typeParams, Properties.CustomType(constructors)) => Some((typeParams, constructors))
        case _                                                     => None
      }
    }

    type DerivedType[+A] = Spec[Properties.DerivedType, A]
    object DerivedType {
      def apply[A](typeParams: List[Name], properties: Properties.DerivedType[A]): DerivedType[A] =
        Spec(typeParams, properties)
      def unapply[F[+_]](spec: Spec[F, _]): Option[(List[Name], Type[_], FQName, FQName)] = spec match {
        case Spec(typeParams, Properties.DerivedType(baseType, fromBaseType, toBaseType)) =>
          Some((typeParams, baseType, fromBaseType, toBaseType))
        case _ => None
      }
    }
  }
  sealed trait Specification[+A] { self =>
    import Specification.*

    final def properties: Properties[A] =
      self match {
        case TypeAliasSpecification(_, expr)              => Properties.TypeAlias(expr)
        case CustomTypeSpecification(_, ctors)            => Properties.CustomType(ctors)
        case DerivedTypeSpecification(_, derivationProps) => derivationProps
        case OpaqueTypeSpecification(_)                   => Properties.OpaqueType
      }

    final def map[B](f: A => B): Specification[B] =
      self match {
        case spec @ TypeAliasSpecification(_, _)  => TypeAliasSpecification(spec.typeParams, spec.expr.map(f))
        case spec @ OpaqueTypeSpecification(_)    => spec
        case spec @ CustomTypeSpecification(_, _) => spec.copy(ctors = spec.ctors.map(f))
        case spec @ DerivedTypeSpecification(_, _) =>
          val baseType = spec.derivationProps.baseType.map(f)
          val props    = spec.derivationProps.copy(baseType = baseType)
          spec.copy(derivationProps = props)
      }

    def typeParams: List[Name]

  }

  object Specification {
    final case class TypeAliasSpecification[+A](typeParams: List[Name], expr: Type[A]) extends Specification[A]
    final case class OpaqueTypeSpecification(typeParams: List[Name])                   extends Specification[Nothing]
    final case class CustomTypeSpecification[+A](typeParams: List[Name], ctors: Constructors[A])
        extends Specification[A]

    final case class DerivedTypeSpecification[+A](typeParams: List[Name], derivationProps: Properties.DerivedType[A])
        extends Specification[A]

    sealed trait Properties[+A] { self =>
      def map[B](f: A => B): Properties[B] =
        self match {
          case props @ Properties.TypeAlias(_)         => Properties.TypeAlias(props.expr.map(f))
          case props @ Properties.CustomType(_)        => props.copy(ctors = props.ctors.map(f))
          case props @ Properties.DerivedType(_, _, _) => props.copy(baseType = props.baseType.map(f))
          case _                                       => Properties.OpaqueType
        }
    }
    object Properties {
      final case class TypeAlias[+A](expr: Type[A]) extends Properties[A]
      type OpaqueType = Properties.OpaqueType.type
      case object OpaqueType extends Properties[Nothing] {
        type Type[+A] = Properties.OpaqueType.type
      }
      final case class CustomType[+A](ctors: Constructors[A]) extends Properties[A]
      final case class DerivedType[+A](baseType: Type[A], fromBaseType: FQName, toBaseType: FQName)
          extends Properties[A]
    }
  }
}

package org.finos.morphir.internal

import org.finos.morphir.naming.*
trait TypeSpecModule { self: TypeModule =>

  sealed trait TypeSpecification[+A] { self =>
    import TypeSpecification.*

    final def properties: Properties =
      self match {
        case TypeAliasSpecification(_, expr)              => Properties.TypeAlias(expr)
        case CustomTypeSpecification(_, ctors)            => Properties.CustomType(ctors)
        case DerivedTypeSpecification(_, derivationProps) => derivationProps
        case OpaqueTypeSpecification(_)                   => Properties.OpaqueType
      }

    final def map[B](f: A => B): TypeSpecification[B] =
      self match {
        case spec @ TypeAliasSpecification(_, _)  => TypeAliasSpecification(spec.typeParams, spec.expr.map(f))
        case spec @ OpaqueTypeSpecification(_)    => spec
        case spec @ CustomTypeSpecification(_, _) => spec.copy(ctors = spec.ctors.map(f))
        case spec @ DerivedTypeSpecification(_, _) =>
          val baseType = spec.derivationProps.baseType.map(f)
          val props    = spec.derivationProps.copy(baseType = baseType)
          spec.copy(derivationProps = props)
      }

    def typeParams: Vector[Name]

  }

  object TypeSpecification {
    sealed case class TypeAliasSpecification[+A](typeParams: Vector[Name], expr: Type[A]) extends TypeSpecification[A]

    sealed case class OpaqueTypeSpecification(typeParams: Vector[Name]) extends TypeSpecification[Nothing]

    sealed case class CustomTypeSpecification[+A](typeParams: Vector[Name], ctors: TypeConstructors[A])
        extends TypeSpecification[A]

    sealed case class DerivedTypeSpecification[+A](typeParams: Vector[Name], derivationProps: Properties.DerivedType[A])
        extends TypeSpecification[A]

    sealed trait Properties {
      self =>
    }

    object Properties {
      sealed case class TypeAlias[+T](expr: T) extends Properties

      type OpaqueType = Properties.OpaqueType.type

      case object OpaqueType extends Properties {
        type Type = Properties.OpaqueType.type
      }

      sealed case class CustomType[+A](ctors: TypeConstructors[A]) extends Properties
      sealed case class DerivedType[+A](baseType: Type[A], fromBaseType: FQName, toBaseType: FQName)
          extends Properties
    }
  }

  sealed case class TypeConstructors[+A](byName: Map[Name, TypeConstructorArgs[A]]) {
    def map[B](f: A => B): TypeConstructors[B] = TypeConstructors(byName.view.mapValues(_.map(f)).toMap)

    @inline def toMap: Map[Name, TypeConstructorArgs[A]] = byName
  }

  object TypeConstructors {
    def fromCtors[A](
        ctor: (Name, TypeConstructorArgs[A]),
        ctors: (Name, TypeConstructorArgs[A])*
    ): TypeConstructors[A] =
      TypeConstructors((ctor +: ctors).toMap)

    implicit def toMap[A](ctors: TypeConstructors[A]): Map[Name, TypeConstructorArgs[A]] = ctors.byName
  }

  sealed case class TypeConstructorArg[+A](name: Name, tpe: Type[A]) {
    def map[B](f: A => B): TypeConstructorArg[B] = TypeConstructorArg(name, tpe.map(f))
  }

  object TypeConstructorArg {
    def fromTuple[A](arg: (Name, Type[A])): TypeConstructorArg[A]        = TypeConstructorArg(arg._1, arg._2)
    implicit def toTuple[A](arg: TypeConstructorArg[A]): (Name, Type[A]) = (arg.name, arg.tpe)
  }

  sealed case class TypeConstructorArgs[+A](args: List[TypeConstructorArg[A]]) {
    self =>
    def map[B](f: A => B): TypeConstructorArgs[B] = TypeConstructorArgs(self.args.map(_.map(f)))

    def toList: List[TypeConstructorArg[A]] = args
  }

  object TypeConstructorArgs {
    def apply[A](args: TypeConstructorArg[A]*): TypeConstructorArgs[A]                = TypeConstructorArgs(args.toList)
    implicit def toList[A](args: TypeConstructorArgs[A]): List[TypeConstructorArg[A]] = args.args
  }

  sealed case class TypeConstructor[+A](name: Name, args: TypeConstructorArgs[A]) {
    def map[B](f: A => B): TypeConstructor[B] = copy(args = args.map(f))
  }

  def tCtorArg[A](name: Name, tpe: Type[A]): TypeConstructorArg[A] = TypeConstructorArg(name, tpe)
  def tCtorArg[A](pair: (Name, Type[A])): TypeConstructorArg[A]    = TypeConstructorArg.fromTuple(pair)

  def tCtorArgs[A](args: TypeConstructorArg[A]*): TypeConstructorArgs[A]        = TypeConstructorArgs(args.toList)
  def typeCtors[A](ctors: (Name, TypeConstructorArgs[A])*): TypeConstructors[A] = TypeConstructors(ctors.toMap)
}

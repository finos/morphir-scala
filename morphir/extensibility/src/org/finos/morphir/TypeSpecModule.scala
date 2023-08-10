package org.finos.morphir
import org.finos.morphir.naming._
trait TypeSpecModule { self: TypeModule =>

  sealed trait TypeSpecification[+A] { self =>
    import TypeSpecification._

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

    def typeParams: List[Name]

  }

  object TypeSpecification {
    sealed case class TypeAliasSpecification[+A](typeParams: List[Name], expr: Type[A]) extends TypeSpecification[A]

    sealed case class OpaqueTypeSpecification(typeParams: List[Name]) extends TypeSpecification[Nothing]

    sealed case class CustomTypeSpecification[+A](typeParams: List[Name], ctors: TypeConstructors[A])
        extends TypeSpecification[A]

    sealed case class DerivedTypeSpecification[+A](typeParams: List[Name], derivationProps: Properties.DerivedType[A])
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

  sealed trait TypeDefinition

  sealed case class TypeConstructors[+A](byName: Map[Name, TypeConstructor[A]]) {
    def map[B](f: A => B): TypeConstructors[B] = TypeConstructors(byName.view.mapValues(_.map(f)).toMap)

    @inline def toMap: Map[Name, TypeConstructor[A]] = byName
  }

  object TypeConstructors {
    implicit def toMap[A](ctors: TypeConstructors[A]): Map[Name, TypeConstructor[A]] = ctors.byName
  }

  sealed case class TypeConstructorArg[+A](name: Name, tpe: Type[A]) {
    def map[B](f: A => B): TypeConstructorArg[B] = TypeConstructorArg(name, tpe.map(f))
  }

  object TypeConstructorArg {
    implicit def toTuple[A](arg: TypeConstructorArg[A]): (Name, Type[A]) = (arg.name, arg.tpe)
  }

  sealed case class TypeConstructorArgs[+A](args: List[TypeConstructorArg[A]]) {
    self =>
    def map[B](f: A => B): TypeConstructorArgs[B] = TypeConstructorArgs(self.args.map(_.map(f)))

    def toList: List[TypeConstructorArg[A]] = args
  }

  object TypeConstructorArgs {
    implicit def toList[A](args: TypeConstructorArgs[A]): List[TypeConstructorArg[A]] = args.args
  }

  sealed case class TypeConstructor[+A](name: Name, args: TypeConstructorArgs[A]) {
    def map[B](f: A => B): TypeConstructor[B] = copy(args = args.map(f))
  }
}

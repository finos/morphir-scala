package org.finos.morphir.universe.ir
import org.finos.morphir.naming.*
import TypeSpecification.*

final case class TypeSpec[Props[+_], +A](typeParams: List[Name], properties: Props[A])
object TypeSpec {
  type TypeAlias[+A] = TypeSpec[Properties.TypeAlias, A]
  object TypeAlias {
    def apply[A](typeParams: List[Name], properties: Properties.TypeAlias[A]): TypeAlias[A] =
      TypeSpec(typeParams, properties)
    def unapply[F[+_]](spec: TypeSpec[F, _]): Option[(List[Name], Type[_])] = spec match {
      case TypeSpec(typeParams, Properties.TypeAlias(typeExpr)) => Some((typeParams, typeExpr))
      case _                                                    => None
    }
  }

  type OpaqueType = TypeSpec[Properties, Nothing]
  object OpaqueType {
    def apply(typeParams: List[Name]): OpaqueType =
      TypeSpec(typeParams, Properties.OpaqueType)
    def unapply[F[+_]](spec: TypeSpec[F, _]): Option[List[Name]] = spec match {
      case TypeSpec(typeParams, Properties.OpaqueType) => Some(typeParams)
      case _                                           => None
    }
  }

  type CustomType[+A] = TypeSpec[Properties.CustomType, A]
  object CustomType {
    def apply[A](typeParams: List[Name], properties: Properties.CustomType[A]): CustomType[A] =
      TypeSpec(typeParams, properties)
    def unapply[F[+_]](spec: TypeSpec[F, _]): Option[(List[Name], TypeConstructors[_])] = spec match {
      case TypeSpec(typeParams, Properties.CustomType(constructors)) => Some((typeParams, constructors))
      case _                                                         => None
    }
  }

  type DerivedType[+A] = TypeSpec[Properties.DerivedType, A]
  object DerivedType {
    def apply[A](typeParams: List[Name], properties: Properties.DerivedType[A]): DerivedType[A] =
      TypeSpec(typeParams, properties)
    def unapply[F[+_]](spec: TypeSpec[F, _]): Option[(List[Name], Type[_], FQName, FQName)] = spec match {
      case TypeSpec(typeParams, Properties.DerivedType(baseType, fromBaseType, toBaseType)) =>
        Some((typeParams, baseType, fromBaseType, toBaseType))
      case _ => None
    }
  }
}

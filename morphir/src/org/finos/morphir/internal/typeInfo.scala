package org.finos.morphir.internal

import org.finos.morphir.naming._
import org.finos.morphir.universe.ir.{Field, Type}
import org.finos.morphir.util.attribs.Attributes

import org.finos.morphir.universe.ir.{Field, Type}
trait TypeInfoModule { self: TypeSpecModule with TypeDefModule =>

  sealed trait GenericTypeInfo[+A] { self =>
    import GenericTypeInfo._
    import TypeSpecification._

    /// Get the FQName of the type if it is available.
    /// NOTE: Intrinsic types like `Unit`, `Tuple`, `Function`, and un-aliased `Record`s do not have a fully qualified name.
    final def fqName: Option[FQName] = self match {
      case Full(name, _, _, _)     => Some(name)
      case TypeAndSpec(name, _, _) => Some(name)
      case TypeOnly(_, name)       => name
    }

    @inline final def getType: Type[A] = tpe

    @inline final def isIntrinsic: Boolean = tpe.isIntrinsic

    final def isOpaque: Boolean = self match {
      case TypeAndSpec(_, _, OpaqueTypeSpecification(_)) => true
      case _                                             => false
    }
    @inline final def isFull: Boolean = self match {
      case Full(_, _, _, _) => true
      case _                => false
    }

    @inline final def isTypeAndSpec: Boolean = self match {
      case TypeAndSpec(_, _, _) => true
      case _                    => false
    }

    @inline final def isTypeOnly: Boolean = self match {
      case TypeOnly(_, _) => true
      case _              => false
    }

    def tpe: Type[A]
  }

  object GenericTypeInfo {

    sealed case class TypeOnly[+A](tpe: Type[A], name: Option[FQName])                        extends GenericTypeInfo[A]
    sealed case class TypeAndSpec[+A](name: FQName, tpe: Type[A], spec: TypeSpecification[A]) extends GenericTypeInfo[A]
    sealed case class Full[+A](name: FQName, tpe: Type[A], spec: TypeSpecification[A], definition: TypeDefinition[A])
        extends GenericTypeInfo[A]
  }

  type TypeInfo = GenericTypeInfo[Attributes]
  object TypeInfo {

    def fromReference(tpe: Type.Reference[Attributes]): TypeInfo = GenericTypeInfo.TypeOnly(tpe, Some(tpe.typeName))
    def fromReference(fqn: FQName, typeParams: Type[Attributes]*): TypeInfo = {
      val tpe = Type.Reference(Attributes.empty, fqn, typeParams.toList)
      GenericTypeInfo.TypeOnly(tpe, Some(fqn))
    }

    def ofOpaque(fqn: FQName, typeParams: Name*): TypeInfo = {
      val tpe  = Type.Reference(Attributes.empty, fqn, typeParams.map(Type.Variable(Attributes.empty, _)).toList)
      val spec = TypeSpecification.OpaqueTypeSpecification(typeParams.toVector)
      GenericTypeInfo.TypeAndSpec(fqn, tpe, spec)
    }

    object Full {
      def unapply(typeInfo: GenericTypeInfo[Attributes])
          : Option[(FQName, Type[Attributes], TypeSpecification[Attributes], TypeDefinition[Attributes])] =
        typeInfo match {
          case GenericTypeInfo.Full(name, tpe, spec, definition) => Some((name, tpe, spec, definition))
          case _                                                 => None
        }
    }
    object TypeOnly {
      def unapply(typeInfo: GenericTypeInfo[Attributes]): Option[(Type[Attributes], Option[FQName])] = typeInfo match {
        case GenericTypeInfo.TypeOnly(tpe, name) => Some((tpe, name))
        case _                                   => None
      }
    }

    object TypeAndSpec {
      def unapply(typeInfo: GenericTypeInfo[Attributes])
          : Option[(FQName, Type[Attributes], TypeSpecification[Attributes])] =
        typeInfo match {
          case GenericTypeInfo.TypeAndSpec(name, tpe, spec) => Some((name, tpe, spec))
          case _                                            => None
        }
    }
  }

}

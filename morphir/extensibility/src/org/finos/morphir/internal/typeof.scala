package org.finos.morphir.internal
import org.finos.morphir.naming.*
import org.finos.morphir.extensibility.SdkModuleDescriptors.*
import org.finos.morphir.util.attribs.Attributes

trait TypeOfModule { self: TypeModule with TypeInfoModule =>

  /// A type class that gets the Morphir type information for a given Scala type.
  trait TypeOf[T] {
    def apply(): TypeInfo
    final def tpe: Type[Attributes] = typeInfo.tpe
    final def typeInfo: TypeInfo    = apply()
  }

  object TypeOf {
    /// Summon an instance of `TypeOf[T]` if one is available.
    def apply[T](implicit typeOf: TypeOf[T]): TypeOf[T] = typeOf

    implicit val boolean: TypeOf[Boolean] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Bool"))

    implicit val int32: TypeOf[Int] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Int"))

    implicit val int64: TypeOf[Long] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Int"))

    implicit val string: TypeOf[String] = () => TypeInfo.ofOpaque(Morphir.SDK.String.fqn("String"))
  }

}

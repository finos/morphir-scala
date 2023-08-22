package org.finos.morphir.internal
import org.finos.morphir.meta.*
import org.finos.morphir.naming.*
import org.finos.morphir.extensibility.SdkModuleDescriptors.*
import org.finos.morphir.util.attribs.Attributes

trait TypeOfModule extends TypeOfModuleVersionSpecific {
  self: TypeModule with TypeDefModule with TypeSpecModule with TypeInfoModule with AccessControlledModule =>
  import AccessControlled._
  import TypeDefinition._
  import TypeSpecification._

  /// A type class that gets the Morphir type information for a given Scala type.
  trait TypeOf[A] {
    def apply(): TypeInfo

    @inline final def fqName: Option[FQName]    = typeInfo.fqName
    @inline final def getType: Type[Attributes] = typeInfo.tpe
    @inline final def tpe: Type[Attributes]     = typeInfo.tpe
    @inline lazy val typeInfo: TypeInfo         = apply()
  }

  object TypeOf extends TypeOfCompanionVersionSpecific {
    /// Summon an instance of `TypeOf[T]` if one is available.
    def apply[T](implicit typeOf: TypeOf[T]): TypeOf[T] = typeOf

    implicit val boolean: TypeOf[Boolean] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Bool"))

    implicit val double: TypeOf[Double] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Float"))

    implicit val float: TypeOf[Float] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Float"))

    implicit val int16: TypeOf[Short] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Int"))

    implicit val int32: TypeOf[Int] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Int"))

    implicit val int64: TypeOf[Long] = () => TypeInfo.ofOpaque(Morphir.SDK.Basics.fqn("Int"))

    implicit val string: TypeOf[String] = () => TypeInfo.ofOpaque(Morphir.SDK.String.fqn("String"))

    implicit def option[A](implicit typeOf: TypeOf[A]): TypeOf[Option[A]] = {
      val fqn = Morphir.SDK.Basics.fqn("Maybe")
      val ctors = typeCtors(
        n"Just"    -> tCtorArgs(tCtorArg(n"value", Type.Variable(Attributes.empty, n"a"))),
        n"Nothing" -> tCtorArgs()
      )
      val definition: TypeDefinition[Attributes] =
        CustomTypeDefinition(Vector.empty, AccessControlled.publicAccess(ctors))
      val spec: TypeSpecification[Attributes] = CustomTypeSpecification(Vector.empty, ctors)

      () =>
        GenericTypeInfo.Full(
          fqn,
          Type.Reference(Attributes.empty, fqn, List(typeOf().tpe)),
          spec,
          definition
        )
    }
    implicit def list[A](implicit typeOfElement: TypeOf[A]): TypeOf[List[A]] = {
      val fqn     = Morphir.SDK.List.fqn("List")
      val typeRef = Type.Reference(Attributes.empty, fqn, typeOfElement.getType)
      () => GenericTypeInfo.TypeAndSpec(fqn, typeRef, TypeSpecification.OpaqueTypeSpecification(Vector.empty))

    }
  }

  def typeof[A](implicit typeOfInstance: TypeOf[A]): TypeOf[A] = typeOfInstance

}

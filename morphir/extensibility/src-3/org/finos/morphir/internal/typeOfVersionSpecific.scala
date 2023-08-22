package org.finos.morphir.internal
import magnolia1._
import org.finos.morphir.naming._
import org.finos.morphir.util.attribs.Attributes

trait TypeOfModuleVersionSpecific { self: TypeModule with TypeOfModule with TypeInfoModule =>
  trait TypeOfCompanionVersionSpecific extends AutoDerivation[TypeOf] {
    def join[T](ctx: CaseClass[TypeOf, T]): TypeOf[T] = {
      val fields = ctx.parameters.map { param =>
        val name                  = Name.fromString(param.label)
        val tpe: Type[Attributes] = param.typeclass.typeInfo.tpe
        Field(name, tpe)
      }.toList
      val recordType: Type[Attributes] = Type.Record(Attributes.empty, fields)
      // TODO: Get more details for this
      () => GenericTypeInfo.TypeOnly(recordType, None)
    }
    def split[T](sealedTrait: SealedTrait[TypeOf, T]): TypeOf[T] = new TypeOf[T] {
      def apply() =
        // TODO: Get this working
        GenericTypeInfo.TypeOnly(Type.Unit(Attributes.empty), None)
    }

    extension [A: TypeOf](a: A) {
      def typeOf: TypeInfo = TypeOf[A].typeInfo
    }
  }
}

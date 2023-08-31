package org.finos.morphir.internal
import magnolia1._
import org.finos.morphir.annotation.qualifiedModuleName
import org.finos.morphir.naming._
import org.finos.morphir.util.attribs.Attributes
trait TypeOfModuleVersionSpecific {
  self: TypeModule with TypeSpecModule with TypeDefModule with TypeOfModule with TypeInfoModule =>

  import TypeDefinition._
  import TypeSpecification._

  trait TypeOfCompanionVersionSpecific {
    final type Typeclass[T] = TypeOf[T]
    implicit def gen[T]: TypeOf[T] = macro Magnolia.gen[T]
    def join[T](ctx: CaseClass[TypeOf, T]): TypeOf[T] = {
      val fields = ctx.parameters.map { param =>
        val name                  = Name.fromString(param.label)
        val tpe: Type[Attributes] = param.typeclass.typeInfo.tpe
        Field(name, tpe)
      }.toList
      val annotations = ctx.annotations ++ ctx.inheritedAnnotations ++ ctx.typeAnnotations
      val fqName: Option[FQName] = annotations.collectFirst {
        case qualifiedModuleName(moduleName) => moduleName % ctx.typeName.short
      }

      val recordType: Type[Attributes] = Type.Record(Attributes.empty, fields)
      // TODO: Get more details for this
      fqName match {
        case None => () =>
            // in this case we have no way of mapping this to a type alias, so we just return a type only containing the
            // record type
            GenericTypeInfo.TypeOnly(recordType, None)
        case Some(fqName) =>
          // Given that we know the FQName for this type, we are able to return a type alias
          val spec = TypeAliasSpecification(Vector.empty /* Need to fill out type params if any*/, recordType)
          val defn = TypeAliasDefinition(Vector.empty, recordType)
          () => GenericTypeInfo.Full(fqName, recordType, spec, defn)
      }

    }

    def split[T](sealedTrait: SealedTrait[TypeOf, T]): TypeOf[T] = new TypeOf[T] {
      def apply() =
        // TODO: Get this working
        GenericTypeInfo.TypeOnly(Type.Unit(Attributes.empty), None)
    }

  }
}

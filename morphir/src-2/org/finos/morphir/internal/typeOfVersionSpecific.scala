package org.finos.morphir.internal
import magnolia1._
import org.finos.morphir.annotation._
import org.finos.morphir.naming._
import org.finos.morphir.util.attribs.Attributes
import org.finos.morphir.universe.ir.{FieldT, Type}

trait TypeOfModuleVersionSpecific {
  self: TypeSpecModule with TypeDefModule with TypeOfModule with TypeInfoModule =>

  import TypeDefinition._
  import TypeSpecification._

  trait TypeOfCompanionVersionSpecific {
    final type Typeclass[T] = TypeOf[T]
    implicit def gen[T]: TypeOf[T] = macro Magnolia.gen[T]
    def join[T](ctx: CaseClass[TypeOf, T]): TypeOf[T] = {
      val fields = ctx.parameters.map { param =>
        val name                  = Name.fromString(param.label)
        val tpe: Type[Attributes] = param.typeclass.typeInfo.tpe
        FieldT(name, tpe)
      }.toList
      val annotations = ctx.annotations ++ ctx.inheritedAnnotations ++ ctx.typeAnnotations
      val fqName: Option[FQName] = annotations.collectFirst {
        case fullyQualifiedName(pkg, mod, local) =>
          val packageName = PackageName.fromString(pkg)
          val moduleName  = ModuleName.fromString(mod)
          val localName   = Name.fromString(local)
          packageName % moduleName % localName
        case qualifiedModuleName(pkg, mod) =>
          val packageName = PackageName.fromString(pkg)
          val moduleName  = ModuleName.fromString(mod)
          val localName   = Name.fromString(ctx.typeName.short)
          packageName % moduleName % localName
        case packageName(pkg) =>
          val packageName = PackageName.fromString(pkg)
          val moduleName  = ModuleName.fromString(ctx.typeName.owner)
          val localName   = Name.fromString(ctx.typeName.short)
          packageName % moduleName % localName
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
      def apply(): TypeInfo =
        // TODO: Get this working
        GenericTypeInfo.TypeOnly(Type.Unit(Attributes.empty), None)
    }

  }
}

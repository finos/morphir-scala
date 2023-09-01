package org.finos.morphir.internal
import magnolia1.{TypeInfo => TypeName, _}
import org.finos.morphir.annotation._
import org.finos.morphir.naming._
import org.finos.morphir.util.attribs.Attributes
import scala.deriving.Mirror
trait TypeOfModuleVersionSpecific {
  self: AccessControlledModule with TypeModule with TypeSpecModule with TypeDefModule with TypeOfModule
    with TypeInfoModule =>
  import TypeDefinition._
  import TypeSpecification._

  trait TypeOfCompanionVersionSpecific extends Derivation[TypeOf] {
    def join[T](ctx: CaseClass[TypeOf, T]): TypeOf[T] = {
      val fields = ctx.parameters.map { param =>
        val name                  = Name.fromString(param.label)
        val tpe: Type[Attributes] = param.typeclass.typeInfo.tpe
        Field(name, tpe)
      }.toList
      val annotations                  = ctx.annotations ++ ctx.inheritedAnnotations ++ ctx.typeAnnotations
      val fqName: Option[FQName]       = deriveFQName(ctx.typeInfo, annotations)
      val recordType: Type[Attributes] = Type.Record(Attributes.empty, fields)
      // TODO: Get more details for this
      fqName match {
        case None => () =>
            // in this case we have no way of mapping this to a type alias, so we just return a type only containing the
            // record type
            GenericTypeInfo.TypeOnly(recordType, None)
        case Some(fqName) =>
          // Given that we know the FQName for this type, we are able to return a type alias
          val spec = TypeAliasSpecification(Vector.empty /*TODO: Need to fill out type params if any*/, recordType)
          val defn = TypeAliasDefinition(Vector.empty /*TODO: Need to fill out type params if any*/, recordType)
          () => GenericTypeInfo.Full(fqName, recordType, spec, defn)
      }

    }
    def split[T](ctx: SealedTrait[TypeOf, T]): TypeOf[T] = new TypeOf[T] {
      def apply() = {
        val cases = ctx.subtypes.map { subtype =>
          val name = Name.fromString(subtype.typeInfo.short)
          val args = TypeConstructorArgs.empty[Attributes]
          TypeConstructor(name, args)
        }

        val ctors                  = TypeConstructors(cases.toMap)
        val annotations            = ctx.annotations ++ ctx.inheritedAnnotations ++ ctx.typeAnnotations
        val fqName: Option[FQName] = deriveFQName(ctx.typeInfo, annotations)

        fqName match {
          case Some(fqName) =>
            val typeParams = Vector.empty // TODO: Need to fill out type params if any
            val typeRef = Type.Reference(
              Attributes.empty,
              fqName,
              typeParams.map(Type.Variable(Attributes.empty, _)).toList
            )

            val spec = TypeSpecification.custom(typeParams, ctors)
            val defn = TypeDefinition.custom(typeParams, ctors)
            GenericTypeInfo.Full(fqName, typeRef, spec, defn)
          case None =>
            ???
        }
      }
    }

    def deriveFQName(typeInfo: TypeName, annotations: IArray[Any]): Option[FQName] = annotations.collectFirst {
      case fullyQualifiedName(pkg, mod, local) =>
        val packageName = PackageName.fromString(pkg)
        val moduleName  = ModuleName.fromString(mod)
        val localName   = Name.fromString(local)
        packageName % moduleName % localName
      case qualifiedModuleName(pkg, mod) =>
        val packageName = PackageName.fromString(pkg)
        val moduleName  = ModuleName.fromString(mod)
        val localName   = Name.fromString(typeInfo.short)
        packageName % moduleName % localName
      case packageName(pkg) =>
        val packageName = PackageName.fromString(pkg)
        val moduleName  = ModuleName.fromString(typeInfo.owner)
        val localName   = Name.fromString(typeInfo.short)
        packageName % moduleName % localName
    }

    inline given gen[A](using Mirror.Of[A]): TypeOf[A] = derived

    extension [A: TypeOf](a: A) {
      def typeOf: TypeInfo = TypeOf[A].typeInfo
    }
  }
}

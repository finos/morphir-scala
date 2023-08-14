package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import zio.Chunk

object Extractors {

  object FQString {
    def unapply(fqName: FQName): Option[String] = Some(fqName.toString())
  }
  object Types {
    object ListRef {
      def unapply(tpe: UType): Option[UType] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:List:list"), Chunk(elementType)) =>
            Some(elementType)
          case _ => None
        }
    }
    object MaybeRef {
      def unapply(tpe: UType): Option[UType] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:Maybe:maybe"), Chunk(elementType)) =>
            Some(elementType)
          case _ => None
        }
    }

    object ResultRef {
      def unapply(tpe: UType): Option[(UType, UType)] =
        tpe match {
          case Type.Reference(attributes, FQString("Morphir.SDK:Result:result"), Chunk(keyType, valType)) =>
            Some((keyType, valType))
          case _ => None
        }
    }
    object DictRef {
      def unapply(tpe: UType): Option[(UType, UType)] =
        tpe match {
          case Type.Reference(attributes, FQString("Morphir.SDK:Dict:dict"), Chunk(keyType, valType)) =>
            Some((keyType, valType))
          case _ => None
        }
    }
    trait CommonReference {
      val tpe: UType
      def unapply(argTpe: UType): Boolean =
        argTpe match {
          case Type.Reference(_, fqName, Chunk()) if fqName == tpe.asInstanceOf[Type.Reference[Unit]].typeName => true
          case _                                                                                               => false
        }
    }
    object IntRef extends CommonReference {
      final val tpe = Basics.intType
    }
    object Int32Ref extends CommonReference {
      final val tpe = sdk.Int.int32Type
    }
    object BoolRef extends CommonReference {
      final val tpe = Basics.boolType
    }
    object FloatRef extends CommonReference {
      final val tpe = Basics.floatType
    }
    object DecimalRef extends CommonReference {
      final val tpe = sdk.Decimal.decimalType
    }
    object StringRef extends CommonReference {
      final val tpe = sdk.String.stringType
    }
    object CharRef extends CommonReference {
      final val tpe = sdk.Char.charType
    }
    object LocalDateRef extends CommonReference {
      final val tpe = sdk.LocalDate.localDateType
    }
    object LocalTimeRef extends CommonReference {
      final val tpe = sdk.LocalTime.localTimeType
    }
    // Matches references to known SDK-defined types
    object SDKRef {
      def unapply(tpe: UType): Boolean = tpe match {
        case IntRef()        => true
        case Int32Ref()      => true
        case BoolRef()       => true
        case FloatRef()      => true
        case StringRef()     => true
        case CharRef()       => true
        case LocalDateRef()  => true
        case LocalTimeRef()  => true
        case ListRef(_)      => true
        case MaybeRef(_)     => true
        case DictRef(_, _)   => true
        case ResultRef(_, _) => true
        case _               => false
      }
    }
    // Extractor object that unwraps a single layer of aliasing, and gives any type names that were bound in the process
    class Dealiased(dists: Distributions) {
      def unapply(tpe: UType): Option[(UType, Map[Name, UType])] = // If it's aliased we may need to grab bindings
        tpe match {
          case SDKRef() => None
          case Type.Reference(_, typeName, typeArgs) =>
            val lookedUp = dists.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
            lookedUp match {
              case Some(T.Specification.TypeAliasSpecification(typeParams, expr)) =>
                val newBindings = typeParams.zip(typeArgs).toMap
                Some(expr, newBindings)
              case _ => None
            }
          case _ => None
        }
    }
  }
  object Values {
    object SomeConstructor {
      def unapply(value: TypedValue): Option[TypedValue] =
        value match {
          case Value.Apply(attributes, Value.Constructor(_, FQString("Morphir.SDK:Maybe:just")), something) =>
            Some(something)
          case _ => None
        }
    }
    object NoneConstructor {
      def unapply(value: TypedValue): Boolean =
        value match {
          case Value.Constructor(_, FQString("Morphir.SDK:Maybe:nothing")) =>
            true
          case _ => false
        }
    }
  }
}

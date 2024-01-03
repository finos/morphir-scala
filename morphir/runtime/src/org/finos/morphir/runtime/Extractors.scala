package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import zio.Chunk
import Extractors.Types

object Extractors {

  object FQString {
    def unapply(fqName: FQName): Option[String] = Some(fqName.toString())
  }
  object Types {
    object ListRef {
      def unapply(tpe: UType): Option[UType] =
        tpe match {
          // TODO: The SDK specification should make these names available, without requiring a type argument
          case Type.Reference(_, FQString("Morphir.SDK:List:list"), List(elementType)) =>
            Some(elementType)
          case _ => None
        }
    }
    object SetRef {
      def unapply(tpe: UType): Option[UType] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:Set:set"), List(elementType)) =>
            Some(elementType)
          case _ => None
        }
    }
    object MaybeRef {
      def unapply(tpe: UType): Option[UType] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:Maybe:maybe"), List(elementType)) =>
            Some(elementType)
          case _ => None
        }
    }

    object ResultRef {
      def unapply(tpe: UType): Option[(UType, UType)] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:Result:result"), List(keyType, valType)) =>
            Some((keyType, valType))
          case _ => None
        }
    }
    object DictRef {
      def unapply(tpe: UType): Option[(UType, UType)] =
        tpe match {
          case Type.Reference(_, FQString("Morphir.SDK:Dict:dict"), List(keyType, valType)) =>
            Some((keyType, valType))
          case _ => None
        }
    }
    trait CommonReference {
      val tpe: UType
      // TODO: Consider exposing more at the SDK level, so that these type names may be looked up w/o the "asInstanceOf"
      def ref = tpe.asInstanceOf[Type.Reference[Unit]]
      def unapply(argTpe: UType): Boolean =
        argTpe match {
          case Type.Reference(_, fqName, Nil) if fqName == ref.typeName => true
          case _                                                        => false
        }
    }
    object OrderRef extends CommonReference{
      final val tpe = Basics.orderType
    }

    object IntRef extends CommonReference {
      final val tpe = Basics.intType
    }
    object Int16Ref extends CommonReference {
      final val tpe = sdk.Int.int16Type
    }
    object Int32Ref extends CommonReference {
      final val tpe = sdk.Int.int32Type
    }
    object Int64Ref extends CommonReference {
      final val tpe = sdk.Int.int64Type
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
    object MonthRef extends CommonReference {
      final val tpe = sdk.LocalDate.monthType
    }
    object DayOfWeekRef extends CommonReference {
      final val tpe = sdk.LocalDate.dayOfWeekType
    }
    // Matches anything w/o nested subtypes
    object LeafType {
      def unapply(tpe: UType): Boolean =
        tpe match {
          case Type.Reference(_, _, Nil) => true
          case Type.Unit(_)              => true
          case _                         => false
        }
    }
    // Matches any reference that does not come from the morphir SDK
    object NonNativeRef {
      def unapply(tpe: UType): Option[(FQName, List[UType])] =
        tpe match {
          case Type.Reference(_, name, args)
              if (!Utils.isNative(name)) =>
            Some((name, args))
          case _ => None
        }
    }
    // Matches any reference that does come from the DK
    object NativeRef {
      def unapply(tpe: UType): Option[(FQName, List[UType])] =
        tpe match {
          case Type.Reference(_, name, args)
              if (Utils.isNative(name)) =>
            Some((name, args))
          case _ => None
        }
    }
    // Extractor object that unwraps a single layer of aliasing, applying any bindings that were discovered in the process
    class Dealiased(dists: Distributions) {
      def unapply(tpe: UType): Option[UType] = // If it's aliased we may need to grab bindings
        tpe match {
          case NativeRef(_, _) => None
          case Type.Reference(_, typeName, typeArgs) =>
            val lookedUp = dists.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
            lookedUp match {
              case Right(T.Specification.TypeAliasSpecification(typeParams, expr)) =>
                val newBindings = typeParams.zip(typeArgs).toMap
                Some(Utils.applyBindings(expr, newBindings))
              case _ => None // Missing name, but failing extractors cause problems
            }
          case _ => None
        }
    }

    // Extractor object that uncurries a function type, dealiasing along the way to reeturn a result value and flat list of arguments
    class CurriedOnion(dists: Distributions) {
      val dealiaser = new Dealiased(dists)
      def unapply(tpe: UType): Option[(UType, List[UType])] = {
        val myself = this
        tpe match {
          case Type.Function(_, arg, myself(inner, args)) =>
            Some((inner, args :+ arg))
          case dealiaser(myself(inner, args)) =>
            Some((inner, args))
          case other =>
            Some((other, List()))
        }
      }
    }
  }
  object Values {
    // Extractor that helps handle currying
    object ApplyChain {
      def unapply(value: TypedValue): Option[(TypedValue, List[TypedValue])] = value match {
        case Value.Apply(_, ApplyChain(inner, args), arg) => Some((inner, args :+ arg))
        case other                                        => Some((other, List()))
      }
    }
    object JustConstructor {
      def unapply(value: TypedValue): Option[TypedValue] =
        value match {
          case Value.Apply(_, Value.Constructor(_, FQString("Morphir.SDK:Maybe:just")), something) =>
            Some(something)
          case _ => None
        }
    }
    object NothingConstructor {
      def unapply(value: TypedValue): Boolean =
        value match {
          case Value.Constructor(_, FQString("Morphir.SDK:Maybe:nothing")) =>
            true
          case _ => false
        }
    }

    object NonNativeRef {
      def unapply(value: TypedValue): Option[(UType, FQName)] =
        value match {
          case Value.Reference(tpe, name)
              if (!Utils.isNative(name)) =>
            Some((tpe, name))
          case _ => None
        }
    }

    object NativeRef {
      def unapply(value: TypedValue): Option[(UType, FQName)] =
        value match {
          case Value.Reference(tpe, name)
              if (Utils.isNative(name)) =>
            Some((tpe, name))
          case _ => None
        }
    }
  }
}

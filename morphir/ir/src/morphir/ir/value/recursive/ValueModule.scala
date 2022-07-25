package morphir.ir.value.recursive

import morphir.ir.value.PatternConstructors
import morphir.ir.{FQName, Name}

trait ValueModule extends ValueConstructors with PatternConstructors {

  import ValueModule.MapValueAttributesPartiallyApplied

  final type Definition[+TA, +VA] = morphir.ir.value.recursive.Definition[TA, VA]
  final val Definition: morphir.ir.value.recursive.Definition.type = morphir.ir.value.recursive.Definition

  final type ValueDefinition[+TA, +VA] = morphir.ir.value.recursive.Definition[TA, VA]
  final val ValueDefinition: morphir.ir.value.recursive.Definition.type = morphir.ir.value.recursive.Definition

  final type Pattern[+A] = morphir.ir.value.Pattern[A]
  final val Pattern: morphir.ir.value.Pattern.type = morphir.ir.value.Pattern

  final type RawValue = morphir.ir.value.recursive.Value.RawValue
  final val RawValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value

  final type Specification[+A] = morphir.ir.value.Specification[A]
  final val Specification: morphir.ir.value.Specification.type = morphir.ir.value.Specification

  final type TypedValue = morphir.ir.value.recursive.Value.TypedValue
  final val TypedValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value.TypedValue

  final type USpecification = morphir.ir.value.USpecification
  final val USpecification: morphir.ir.value.USpecification.type = morphir.ir.value.USpecification

  final type Value[+TA, +VA] = morphir.ir.value.recursive.Value[TA, VA]
  final val Value: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value

  final def collectReferences[TA, VA](value: Value[TA, VA]): Set[FQName] = value.collectReferences
  final def collectVariables[TA, VA](value: Value[TA, VA]): Set[Name]    = value.collectVariables

  final def mapValueAttributes[TA, TB, VA, VB](f: TA => TB, g: VA => VB, value: Value[TA, VA]): Value[TB, VB] =
    value.mapAttributes(f, g)

  final def mapValueAttributes[TA, VA](value: Value[TA, VA]): MapValueAttributesPartiallyApplied[TA, VA] =
    new MapValueAttributesPartiallyApplied(value)

  final def patternAttribute[A](pattern: Pattern[A]): A = pattern.attributes

  final def toRawValue[TA, VA](value: Value[TA, VA]): RawValue = value.toRawValue

  final def uncurryApply[TA, VA](
      fun: Value[TA, VA],
      lastArg: Value[TA, VA]
  ): (Value[TA, VA], scala.List[Value[TA, VA]]) =
    fun.uncurryApply(lastArg)

  final def valueAttribute[VA](value: Value[Nothing, VA]): VA = value.attributes
}

object ValueModule extends ValueModule {
  final class MapValueAttributesPartiallyApplied[TA, VA](val value: Value[TA, VA]) extends AnyVal {
    def apply[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] =
      value.mapAttributes(f, g)
  }
}

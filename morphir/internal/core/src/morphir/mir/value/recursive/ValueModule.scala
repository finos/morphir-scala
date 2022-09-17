package org.finos
package morphir
package mir
package value.recursive

import org.finos.morphir.mir.value.PatternConstructors
import org.finos.morphir.mir.{FQName, Name}

trait ValueModule extends ValueConstructors with PatternConstructors {

  import ValueModule.MapValueAttributesPartiallyApplied

  final type Definition[+TA, +VA] = morphir.mir.value.recursive.Definition[TA, VA]
  final val Definition: morphir.mir.value.recursive.Definition.type = morphir.mir.value.recursive.Definition

  final type ValueDefinition[+TA, +VA] = morphir.mir.value.recursive.Definition[TA, VA]
  final val ValueDefinition: morphir.mir.value.recursive.Definition.type = morphir.mir.value.recursive.Definition

  final type Pattern[+A] = morphir.mir.value.Pattern[A]
  final val Pattern: morphir.mir.value.Pattern.type = morphir.mir.value.Pattern

  final type RawValue = morphir.mir.value.recursive.Value.RawValue
  final val RawValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value

  final type Specification[+A] = morphir.mir.value.Specification[A]
  final val Specification: morphir.mir.value.Specification.type = morphir.mir.value.Specification

  final type TypedValue = morphir.mir.value.recursive.Value.TypedValue
  final val TypedValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value.TypedValue

  final type USpecification = morphir.mir.value.USpecification
  final val USpecification: morphir.mir.value.USpecification.type = morphir.mir.value.USpecification

  final type Value[+TA, +VA] = morphir.mir.value.recursive.Value[TA, VA]
  final val Value: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value

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

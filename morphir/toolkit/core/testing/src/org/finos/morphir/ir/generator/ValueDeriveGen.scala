package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Value.Value
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ValueDeriveGen {
  implicit def applyValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Apply[TA, VA]] =
    DeriveGen.instance(ValueGen.applyFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def constructorValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.Constructor[VA]] =
    DeriveGen.instance(ValueGen.constructorFromAttributes(DeriveGen[VA]))

  implicit def destructureValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Destructure[TA, VA]] =
    DeriveGen.instance(ValueGen.destructureFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def fieldValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Field[TA, VA]] =
    DeriveGen.instance(ValueGen.fieldFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def fieldFunctionValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.FieldFunction[VA]] =
    DeriveGen.instance(ValueGen.fieldFunctionFromAttributes(DeriveGen[VA]))

  implicit def ifThenElseValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.IfThenElse[TA, VA]] =
    DeriveGen.instance(ValueGen.ifThenElseFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def lambdaValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Lambda[TA, VA]] =
    DeriveGen.instance(ValueGen.lambdaFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def letDefinitionValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.LetDefinition[TA, VA]] =
    DeriveGen.instance(ValueGen.letDefinitionFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def letRecursionValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.LetRecursion[TA, VA]] =
    DeriveGen.instance(ValueGen.letRecursionFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def listValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.List[TA, VA]] =
    DeriveGen.instance(ValueGen.listFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def literalValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.Literal[VA]] =
    DeriveGen.instance(ValueGen.literalFromAttributes(DeriveGen[VA]))

  implicit def patternMatchValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.PatternMatch[TA, VA]] =
    DeriveGen.instance(ValueGen.patternMatchFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def recordValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Record[TA, VA]] =
    DeriveGen.instance(ValueGen.recordValueFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def referenceValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.Reference[VA]] =
    DeriveGen.instance(ValueGen.referenceValueFromAttributes(DeriveGen[VA]))

  implicit def tupleValueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value.Tuple[TA, VA]] =
    DeriveGen.instance(ValueGen.tupleValueFromAttributes(DeriveGen[TA], DeriveGen[VA]))

  implicit def unitValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.Unit[VA]] =
    DeriveGen.instance(ValueGen.unitValue(DeriveGen[VA]))

  implicit def variableValueDeriveGen[VA: DeriveGen]: DeriveGen[Value.Variable[VA]] =
    DeriveGen.instance(ValueGen.variableValueFromAttributes(DeriveGen[VA]))

  implicit def valueDeriveGen[TA: DeriveGen, VA: DeriveGen]: DeriveGen[Value[TA, VA]] =
    DeriveGen.instance(ValueGen.value(DeriveGen[TA], DeriveGen[VA]))
}

object ValueDeriveGen extends ValueDeriveGen

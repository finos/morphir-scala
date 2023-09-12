package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Type
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait TypeDeriveGen {
  implicit def extensibleRecordTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.ExtensibleRecord[A]] =
    DeriveGen.instance(TypeGen.extensibleRecordFromAttributes(DeriveGen[A]))

  implicit def functionTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Function[A]] =
    DeriveGen.instance(TypeGen.functionFromAttributes(DeriveGen[A]))

  implicit def recordTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Record[A]] =
    DeriveGen.instance(TypeGen.recordTypeFromAttributes(DeriveGen[A]))

  implicit def referenceTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Reference[A]] =
    DeriveGen.instance(TypeGen.referenceTypeFromAttributes(DeriveGen[A]))

  implicit def tupleTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Tuple[A]] =
    DeriveGen.instance(TypeGen.tupleTypeFromAttributes(DeriveGen[A]))

  implicit def unitTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Unit[A]] =
    DeriveGen.instance(TypeGen.unitType(DeriveGen[A]))

  implicit def varialbleTypeDeriveGen[A: DeriveGen]: DeriveGen[Type.Variable[A]] =
    DeriveGen.instance(TypeGen.variableTypeFromAttributes(DeriveGen[A]))

  implicit def typeDeriveGen[A: DeriveGen]: DeriveGen[Type[A]] =
    DeriveGen.instance(TypeGen.typeGen(DeriveGen[A]))
}

object TypeDeriveGen extends TypeDeriveGen

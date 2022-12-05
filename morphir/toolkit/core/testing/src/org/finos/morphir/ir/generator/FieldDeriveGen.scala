package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait FieldDeriveGen {
  implicit def fieldDeriveGen[A: DeriveGen]: DeriveGen[Field[A]] =
    DeriveGen.instance(FieldGen.fieldFromAttributes(DeriveGen[A]))
}

object FieldDeriveGen extends FieldDeriveGen

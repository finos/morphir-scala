package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait DocumentedDeriveGen {
  implicit def documentedDeriveGen[A: DeriveGen]: DeriveGen[Documented[A]] =
    DeriveGen.instance(DocumentedGen.documentedFromAttributes(DeriveGen[A]))
}

object DocumentedDeriveGen extends DocumentedDeriveGen

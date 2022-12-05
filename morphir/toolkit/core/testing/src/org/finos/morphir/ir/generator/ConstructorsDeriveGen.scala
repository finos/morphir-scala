package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Constructors
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait ConstructorsDeriveGen {
  implicit def constructorsDeriveGen[A: DeriveGen]: DeriveGen[Constructors[A]] =
    DeriveGen.instance(ConstructorsGen.constructorsFromAttributes(DeriveGen[A]))
}

object ConstructorsDeriveGen extends ConstructorsDeriveGen

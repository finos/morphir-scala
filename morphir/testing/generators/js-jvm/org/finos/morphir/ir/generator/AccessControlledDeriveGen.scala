package org.finos.morphir
package ir
package generator

import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait AccessControlledDeriveGen {
  implicit def accessControlledDeriveGen[A: DeriveGen]: DeriveGen[AccessControlled[A]] =
    DeriveGen.instance(AccessControlledGen.accessControlledFromAttributes(DeriveGen[A]))

  implicit val accessDeriveGen: DeriveGen[AccessControlled.Access] =
    DeriveGen.instance(AccessControlledGen.access)
}

object AccessControlledDeriveGen extends AccessControlledDeriveGen

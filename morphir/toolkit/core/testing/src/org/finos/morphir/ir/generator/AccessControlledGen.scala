package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.AccessControlled
import zio.test.Gen

trait AccessControlledGen {
  final val access: Gen[Any, AccessControlled.Access] =
    Gen.elements(AccessControlled.Access.Private, AccessControlled.Access.Public)

  final def accessControlled[R, A](
      accessGen: Gen[R, AccessControlled.Access],
      valueGen: Gen[R, A]
  ): Gen[R, AccessControlled[A]] = for {
    access <- accessGen
    value  <- valueGen
  } yield AccessControlled(access, value)

  final def accessControlledFromAttributes[R, A](implicit valueGen: Gen[R, A]): Gen[R, AccessControlled[A]] =
    accessControlled(access, valueGen)
}

object AccessControlledGen extends AccessControlledGen

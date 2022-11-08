package org.finos.morphir
package ir
package generator

import zio.test.Gen
import org.finos.morphir.ir.FQName
import org.finos.morphir.ir.Value.{Value => V}

trait ValueGen {
  final def reference[R, VA](attributes: Gen[R, VA], fullyQualifiedName: Gen[R, FQName]) =
    for {
      a   <- attributes
      fqn <- fullyQualifiedName
    } yield V.Reference(a, fqn)
}

object ValueGen extends ValueGen

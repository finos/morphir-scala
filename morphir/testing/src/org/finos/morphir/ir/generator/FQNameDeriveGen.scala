package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait FQNameDeriveGen {
  implicit val fqNameDeriveGen: DeriveGen[FQName] = DeriveGen.instance(FQNameGen.fqName)
}

object FQNameDeriveGen extends FQNameDeriveGen

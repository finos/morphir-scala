package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
2.1.26import zio.test.magnolia.DeriveGen._

trait NameDeriveGen {
  implicit val nameDeriveGen: DeriveGen[Name] = DeriveGen.instance(NameGen.name)
}

object NameDeriveGen extends NameDeriveGen

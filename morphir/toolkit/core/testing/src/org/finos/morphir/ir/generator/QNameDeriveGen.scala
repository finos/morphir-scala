package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait QNameDeriveGen {
  implicit val qNameDeriveGen: DeriveGen[QName] = DeriveGen.instance(QNameGen.qName)
}

object QNameDeriveGen extends QNameDeriveGen

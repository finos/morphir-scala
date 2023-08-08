package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

object PathDeriveGen extends PathDeriveGen
trait PathDeriveGen {
  implicit val pathDeriveGen: DeriveGen[Path] = DeriveGen.instance(PathGen.path)
}

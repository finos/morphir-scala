package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.magnolia.DeriveGen
2.1.26import zio.test.magnolia.DeriveGen._

object PathDeriveGen extends PathDeriveGen
trait PathDeriveGen {
  implicit val pathDeriveGen: DeriveGen[Path] = DeriveGen.instance(PathGen.path)
}

package org.finos
package morphir
package testing

import zio.json.*

object VerifierSpec extends MorphirBaseSpec with SnapshotSpec {
  def spec = suite("VerifierSpec")(
    snapshotTest("SnapshotTest001") {
      List(1, 2, 3)
    }
  )
}

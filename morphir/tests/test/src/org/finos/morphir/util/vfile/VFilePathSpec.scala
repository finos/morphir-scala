package org.finos.morphir.util.vfile

import java.nio.file.Paths
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object VFilePathSpec extends MorphirBaseSpec with VFilePathSpecPlatformSpecific {
  def spec = suite("VfilePathSpec")(
    platformSpecificSuite
  )
}

package org.finos.morphir.util.vfile

import java.nio.file.Paths
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object VPathSpec extends MorphirBaseSpec with VPathSpecPlatformSpecific {
  def spec = suite("VPathSpec")(
    platformSpecificSuite
  )
}

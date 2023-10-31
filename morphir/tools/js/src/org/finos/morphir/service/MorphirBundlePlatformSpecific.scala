package org.finos.morphir.service

import org.finos.morphir.util.vfile.*
import zio.json.*

trait MorphirBundlePlatformSpecific {
  val live: ULayer[MorphirBundle] = ZLayer.succeed(MorphirBundleLive)

}

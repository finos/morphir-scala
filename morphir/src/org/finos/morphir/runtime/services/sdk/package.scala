package org.finos.morphir.runtime.services
import zio.ZEnvironment

package object sdk {
  type MorphirSdk = BasicsModule with IntModule with LocalDateModule
  object MorphirSdk {
    def live: ZEnvironment[MorphirSdk] = ZEnvironment[BasicsModule, IntModule, LocalDateModule](
      BasicsModule.live,
      IntModule.live,
      LocalDateModule.live
    )
  }
}

package org.finos.morphir.runtime.services
import zio.ZEnvironment

package object sdk {
  type MorphirSdk = BasicsModule with IntModule
  object MorphirSdk {
    def live: ZEnvironment[MorphirSdk] = ZEnvironment[BasicsModule, IntModule](
      BasicsModule.live,
      IntModule.live
    )
  }
}

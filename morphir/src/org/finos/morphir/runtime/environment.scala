package org.finos.morphir.runtime
import org.finos.morphir.runtime.services.kernel.MorphirKernel
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import zio.*

object environment {
  type MorphirEnv = MorphirSdk with MorphirKernel
  object MorphirEnv {
    def live: ZEnvironment[MorphirEnv] = MorphirSdk.live ++ MorphirKernel.live
  }
}

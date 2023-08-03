package org.finos.morphir.runtime.services
import zio._
package object kernel {
  type MorphirKernel = Kernel
  object MorphirKernel {
    def live: ZEnvironment[MorphirKernel] = ZEnvironment[Kernel](
      Kernel.live
    )
  }
}

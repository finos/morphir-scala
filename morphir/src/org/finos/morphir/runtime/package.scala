package org.finos.morphir
import org.finos.morphir.runtime.services.kernel.MorphirKernel
import org.finos.morphir.runtime.services.sdk.MorphirSdk

package object runtime {
  type URTAction[+A] = RTAction[Any, Nothing, A]
  val URTAction: org.finos.morphir.runtime.RTAction.type = org.finos.morphir.runtime.RTAction

  

  type MorphirEnv = MorphirSdk with MorphirKernel

  type ??? = Nothing
}

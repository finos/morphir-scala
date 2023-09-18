package org.finos.morphir.runtime.services.sdk

import zio.ZEnvironment

class MorphirSdk

// TODO This is superseded by NativeSDK. Perhaps it should be ZIO-Ified?
object MorphirSdk {
  def live: ZEnvironment[MorphirSdk] = ZEnvironment[MorphirSdk](new MorphirSdk())
}

/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.internal.util

/**
 * Ported from ZIO: https://github.com/zio/zio/blob/series/2.x/core/js/src/main/scala/zio/ChunkPlatformSpecific.scala
 */

// import zio.stacktracer.TracingImplicits.disableAutoTrace

import scala.reflect.{ClassTag, classTag}

private[util] trait ChunkPlatformSpecific {

  private[util] object Tags {
    def fromValue[A](a: A): ClassTag[A] = {
      val _ = a
      classTag[AnyRef].asInstanceOf[ClassTag[A]]
    }
  }
}

package org.finos.morphir.runtime.service

import zio._

trait MorphirRuntimeDriver {
  def test(): Task[Unit]
}

object MorphirRuntimeDriver extends MorphirRuntimeDriverPlatformSpecific {

  def test(): ZIO[MorphirRuntimeDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirRuntimeDriver](_.test())
}

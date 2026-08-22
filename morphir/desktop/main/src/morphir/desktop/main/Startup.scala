package morphir.desktop.main

import kyo.*

object Startup:

  def initialize[A, S1, S2](
      assemble: => A < S1,
      install: A => Unit < S2,
      load: => Unit < Sync
  ): Unit < (S1 & S2 & Sync) =
    assemble.map { value =>
      install(value).map { _ => load }
    }

  def failClosed[E: ConcreteTag, S](
      program: => Unit < (Abort[E] & S),
      close: => Unit < Sync
  ): Unit < (S & Sync) =
    Abort.run[E](program).map {
      case Result.Success(_)                   => ()
      case Result.Failure(_) | Result.Panic(_) => close
    }

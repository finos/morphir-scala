package org.finos.morphir.runtime.services.sdk
import org.finos.morphir.runtime.exports.*
import _root_.morphir.sdk.{Basics, LocalDate}
import _root_.morphir.sdk.LocalDate.{LocalDate => MLocalDate}
import zio.ZEnvironment

/// This module adds the definition of a date without time zones. Useful in business modeling.
trait LocalDateModule {
  def addDays(count: Basics.Int, date: MLocalDate): URTAction[MLocalDate]
}

object LocalDateModule {
  val live: LocalDateModule = LocalDateModuleLive()
  val liveEnv: ZEnvironment[LocalDateModule] = ZEnvironment[LocalDateModule](
    LocalDateModule.live
  )
}

final case class LocalDateModuleLive() extends LocalDateModule {

  def addDays(count: Basics.Int, date: MLocalDate): URTAction[MLocalDate] =
    URTAction.succeed(LocalDate.addDays(count)(date))
}

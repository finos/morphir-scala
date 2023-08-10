package org.finos.morphir.runtime

sealed trait EnableTyper
object EnableTyper {
  case object Enabled  extends EnableTyper
  case object Disabled extends EnableTyper
  case object Warn     extends EnableTyper
}

final case class RTExecutionContext(options: RTExecutionContext.Options)
object RTExecutionContext {
  val default: RTExecutionContext = RTExecutionContext(RTExecutionContext.Options.default)

  case class Options(
      enableTyper: EnableTyper
  )
  object Options {
    val default: Options = Options(EnableTyper.Enabled)
  }
}

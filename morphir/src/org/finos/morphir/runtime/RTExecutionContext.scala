package org.finos.morphir.runtime

sealed trait EnableTyper
object EnableTyper {
  case object Enabled  extends EnableTyper
  case object Disabled extends EnableTyper
  case object Warn     extends EnableTyper
}

final case class RTExecutionContext(options: RTExecutionContext.Options)
object RTExecutionContext {
  val default: RTExecutionContext            = RTExecutionContext(RTExecutionContext.Options.default)
  val typeChecked: RTExecutionContext        = default.copy(options = Options.typeChecked)
  val typeCheckedWarning: RTExecutionContext = default.copy(options = Options.typeCheckedWarning)
  val notTypeChecked: RTExecutionContext     = default.copy(options = Options.notTypeChecked)

  case class Options(
      enableTyper: EnableTyper
  )
  object Options {
    val default: Options            = Options(EnableTyper.Enabled)
    val typeChecked: Options        = default.copy(enableTyper = EnableTyper.Enabled)
    val typeCheckedWarning: Options = default.copy(enableTyper = EnableTyper.Warn)
    val notTypeChecked: Options     = default.copy(enableTyper = EnableTyper.Disabled)
  }
}

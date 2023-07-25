package org.finos.morphir
package runtime

sealed trait EngineEvent
sealed trait LogEvent extends EngineEvent
object LogEvent {
  case class Info(message: String)    extends LogEvent
  case class Warning(message: String) extends LogEvent
  case class Error(message: String)   extends LogEvent
  case class Trace(message: String)   extends LogEvent
}

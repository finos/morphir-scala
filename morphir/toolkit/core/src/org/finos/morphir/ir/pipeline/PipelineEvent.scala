package org.finos.morphir.ir.pipeline

sealed trait PipelineEvent extends Product with Serializable

object PipelineEvent {
  sealed abstract class LogEvent(val rawMessage: String) extends PipelineEvent
  object LogEvent {
    final case class Trace(override val rawMessage: String)   extends LogEvent(rawMessage)
    final case class Info(override val rawMessage: String)    extends LogEvent(rawMessage)
    final case class Warning(override val rawMessage: String) extends LogEvent(rawMessage)
    final case class Error(override val rawMessage: String)   extends LogEvent(rawMessage)
  }
}

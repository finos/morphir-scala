package org.finos.morphir.runtime
sealed trait SingleTestResult {}
object SingleTestResult {
  case object Passed             extends SingleTestResult
  case class Failed(msg: String) extends SingleTestResult
  case class Err(e: Throwable)   extends SingleTestResult
}

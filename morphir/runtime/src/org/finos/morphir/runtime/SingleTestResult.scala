package org.finos.morphir.runtime
trait SingleTestResult {}
case object Pass               extends SingleTestResult
case class Failed(msg: String) extends SingleTestResult
case class Err(e: Throwable)   extends SingleTestResult

package org.finos.morphir.runtime

case class TestSummary(
    message: String,
    success: Boolean // How should Incomplete be handled?

)
sealed trait OverallStatus

case object Passed     extends OverallStatus
case object Failed     extends OverallStatus
case object Incomplete extends OverallStatus
case class TestResultCounts(passed: Int, failed: Int, skipped: Int, todo: Int)

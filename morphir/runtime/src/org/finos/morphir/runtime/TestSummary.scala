package org.finos.morphir.runtime
import org.finos.morphir.naming.*

case class TestSummary(
    message: String,
    success: Boolean, // How should Incomplete be handled?
    countsByModule: Map[(pkgName: PackageName, modName: ModuleName), TestResultCounts],
)

sealed trait OverallStatus
object OverallStatus {
  case object Passed     extends OverallStatus
  case object Failed     extends OverallStatus
  case object Incomplete extends OverallStatus
}
case class TestResultCounts(passed: Int, failed: Int, errors: Int, skipped: Int, todo: Int) {
  def plus(other: TestResultCounts) = TestResultCounts(
    passed + other.passed,
    failed + other.failed,
    errors + other.errors,
    skipped + other.skipped,
    todo + other.todo
  )
}

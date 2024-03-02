package org.finos.morphir.runtime
import org.finos.morphir.naming.*

case class TestSummary(
    message: String,
    success: Boolean, // How should Incomplete be handled?
    countsByModule: Map[(pkgName: PackageName, modName: ModuleName), TestResultCounts]
) {
  def overallCounts = countsByModule.values.foldLeft(TestResultCounts.empty) { case (acc, next) => acc.plus(next) }
  def countsAtModule(pkgName: PackageName, modName: ModuleName): Option[TestResultCounts] =
    countsByModule.get(pkgName, modName)
  def result = overallCounts.result
  def resultByModule(pkgName: PackageName, modName: ModuleName): Option[OverallStatus] =
    countsAtModule(pkgName, modName).map(_.result)

}

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
  def result: OverallStatus =
    if (failed > 0 || errors > 0) OverallStatus.Failed
    else if (skipped > 0 || todo > 0) OverallStatus.Incomplete
    else OverallStatus.Passed
}
object TestResultCounts {
  def empty = TestResultCounts(0, 0, 0, 0, 0)
}

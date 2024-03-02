package org.finos.morphir.runtime
import org.finos.morphir.naming.*
import javax.swing.text.PasswordView

case class TestSummary(
    report: String,
    countsByModule: Map[(PackageName, ModuleName), TestResultCounts]
) {
  def overallCounts = countsByModule.values.foldLeft(TestResultCounts.empty) { case (acc, next) => acc.plus(next) }
  def countsAtModule(pkgName: PackageName, modName: ModuleName): Option[TestResultCounts] =
    countsByModule.get(pkgName, modName)
  def result = overallCounts.result
  def resultByModule(pkgName: PackageName, modName: ModuleName): Option[OverallStatus] =
    countsAtModule(pkgName, modName).map(_.result)
  def passed     = result == OverallStatus.Passed
  def incomplete = result == OverallStatus.Incomplete
  // not including a "failed" function in hopes no one assumed !failed == success
  override def toString =
    s"""$report
    $overallCounts
    Overal Status - $result"""
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
  override def toString = s"passed : $passed, failed  $failed, errors : $errors, skipped : $skipped, todo : $todo"
}
object TestResultCounts {
  def empty = TestResultCounts(0, 0, 0, 0, 0)
}

package org.finos.morphir.runtime
import org.finos.morphir.naming.*

private[runtime] case class CoverageInfo(
    staticallyReachedFunctions: Set[FQName],
    userDefinedFunctions: Set[FQName],
    counts: CoverageCounts
)
case class TestSummary(
    report: String,
    countsByModule: Map[(PackageName, ModuleName), TestResultCounts],
    private val coverage: CoverageInfo,
    private val individualTestResults: Map[FQName, SingleTestResult]
) {
  def overallCounts = countsByModule.values.foldLeft(TestResultCounts.empty) { case (acc, next) => acc.plus(next) }
  def countsAtModule(pkgName: PackageName, modName: ModuleName): Option[TestResultCounts] =
    countsByModule.get((pkgName, modName))
  def result = overallCounts.result
  def resultAtModule(pkgName: PackageName, modName: ModuleName): Option[OverallStatus] =
    countsAtModule(pkgName, modName).map(_.result)

  def coverageCounts: CoverageCounts = coverage.counts

  def userDefinedFunctions = coverage.userDefinedFunctions

  def staticallyReachedFunctions = coverage.staticallyReachedFunctions

  def getAllTestResults = individualTestResults

  /**
   * Evaluates to true if and only if all tests passed and none were skipped or todo
   */
  def passed = result == OverallStatus.Passed

  /**
   * Evaluates to true if no tests failed but some were skipped or todo
   */
  def incomplete = result == OverallStatus.Incomplete
  // not including a "failed" function in hopes no one assumed !failed == success
  override def toString =
    s"$report ${System.lineSeparator()}" +
      s"$overallCounts ${System.lineSeparator()}" +
      s"Overall Status - $result ${System.lineSeparator()}" +
      s"Coverage - ${coverage.counts}"
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

case class CoverageCounts(covered: Int, uncovered: Int) {
  def overallCoverage = if (uncovered == 0) 100 else (covered.toFloat / (covered + uncovered).toFloat) * 100

  override def toString =
    s"Covered Functions: $covered, Uncovered Functions: $uncovered Overall Coverage: $overallCoverage %"
}

object CoverageCounts {
  def empty = CoverageCounts(0, 0)

  def getCounts(definedFunctions: Set[FQName], reachedFunctions: Set[FQName]): CoverageCounts = {
    val covered   = definedFunctions.filter(reachedFunctions.contains(_)).size
    val uncovered = definedFunctions.size - covered
    CoverageCounts(covered, uncovered)
  }
}

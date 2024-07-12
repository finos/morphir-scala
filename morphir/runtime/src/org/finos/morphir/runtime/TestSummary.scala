package org.finos.morphir.runtime
import org.finos.morphir.naming.*

private[runtime] case class CoverageInfo(staticallyReachedFunctions: List[FQName], userDefinedFunctions: List[FQName])
case class TestSummary(
    private val coverage: CoverageInfo,
    report: String,
    countsByModule: Map[(PackageName, ModuleName), TestResultCounts]
) {
  def overallCounts = countsByModule.values.foldLeft(TestResultCounts.empty) { case (acc, next) => acc.plus(next) }
  def countsAtModule(pkgName: PackageName, modName: ModuleName): Option[TestResultCounts] =
    countsByModule.get((pkgName, modName))
  def result = overallCounts.result
  def resultAtModule(pkgName: PackageName, modName: ModuleName): Option[OverallStatus] =
    countsAtModule(pkgName, modName).map(_.result)

  /**
   * Package coverage calculation result
   */
  def overallCoverage: Map[Path, Float] = {
    val called    = coverage.userDefinedFunctions.filter(coverage.staticallyReachedFunctions.contains(_))
    val udfByPath = coverage.userDefinedFunctions.groupBy(fqn => fqn.getPackagePath ++ fqn.getModulePath)
    val calledByPath = coverage.userDefinedFunctions.filter(coverage.staticallyReachedFunctions.contains(_)).groupBy(
      fqn => fqn.getPackagePath ++ fqn.getModulePath
    )

    calledByPath.foldLeft(Map.empty[Path, Float]) { case (acc, (pkg, calledByPkg)) =>
      udfByPath.get(pkg) match {
        case Some(udf: List[FQName]) => acc + (pkg -> (calledByPkg.size.toFloat / udf.size.toFloat) * 100)
        case _                       => acc
      }
    }
  }

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
    s"$report\n" +
      s"$overallCounts\n" +
      s"Overall Status - $result\n" +
      s"Coverage by package - $overallCoverage"
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

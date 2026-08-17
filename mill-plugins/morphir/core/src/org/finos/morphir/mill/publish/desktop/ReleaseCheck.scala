package org.finos.morphir.mill.publish.desktop

/**
 * A single named pre-publish check over a [[ReleaseSnapshot]].
 *
 * `find` is pure — no filesystem access, no side effects — so adding a new check is one value in a list
 * ([[DesktopReleaseChecks.all]]) and one test, never a change to how checks run.
 */
final case class ReleaseCheck(name: String, find: ReleaseSnapshot => Seq[String])

/** The outcome of running one [[ReleaseCheck]]: its name, and every problem it found (empty means it passed). */
final case class CheckOutcome(name: String, problems: Seq[String]) {
  def passed: Boolean = problems.isEmpty
}

/** Runs every check against a snapshot and collects every problem, rather than stopping at the first. */
object ReleaseVerifier {
  def run(checks: Seq[ReleaseCheck], snapshot: ReleaseSnapshot): Seq[CheckOutcome] =
    checks.map(check => CheckOutcome(check.name, check.find(snapshot)))
}

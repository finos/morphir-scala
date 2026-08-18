package org.finos.morphir.mill.publish.version

/** A semantic version, parsed far enough to order two of them. */
final case class SemVer(major: Int, minor: Int, patch: Int, prerelease: Option[String])

object SemVer {
  private val Pattern = raw"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9A-Za-z.-]+))?$$".r

  def parse(text: String): Option[SemVer] = text match {
    case Pattern(major, minor, patch, prerelease) if Option(prerelease).forall(validPrerelease) =>
      Some(SemVer(major.toInt, minor.toInt, patch.toInt, Option(prerelease)))
    case _ => None
  }

  /**
   * The character class in `Pattern` is not enough on its own: it admits `1.2.3-alpha..1` (an empty identifier) and
   * `1.2.3-01` (a numeric identifier with a leading zero), both of which semver rejects. A typo of either shape in an
   * undated changelog heading would otherwise pass the changelog gate and reach a published coordinate.
   */
  private def validPrerelease(prerelease: String): Boolean =
    prerelease.split("\\.", -1).forall { identifier =>
      identifier.nonEmpty &&
      (!identifier.forall(_.isDigit) || identifier == "0" || !identifier.startsWith("0"))
    }

  /**
   * Orders two versions, negative when `a` precedes `b`.
   *
   * A prerelease sorts below the release it qualifies, per semver: `0.6.0-M01` precedes `0.6.0`. Prereleases are
   * compared lexically among themselves, which is enough for this repository's `M01`/`M02`/`RC1` shapes and avoids
   * implementing the full dot-separated identifier rules for a case nothing here produces.
   */
  def compare(a: String, b: String): Either[String, Int] =
    (parse(a), parse(b)) match {
      case (None, _)                 => Left(s"'$a' is not a semantic version")
      case (_, None)                 => Left(s"'$b' is not a semantic version")
      case (Some(left), Some(right)) =>
        val numeric =
          Ordering[(Int, Int, Int)].compare(
            (left.major, left.minor, left.patch),
            (right.major, right.minor, right.patch)
          )
        if (numeric != 0) Right(numeric.sign)
        else
          Right((left.prerelease, right.prerelease) match {
            case (None, None)             => 0
            case (None, Some(_))          => 1
            case (Some(_), None)          => -1
            case (Some(one), Some(other)) => one.compareTo(other).sign
          })
    }
}

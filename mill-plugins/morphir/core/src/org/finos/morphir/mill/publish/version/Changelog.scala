package org.finos.morphir.mill.publish.version

/** A version heading in a changelog. `date` absent means the release has not happened yet. */
final case class ChangelogHeading(version: String, date: Option[String])

/**
 * Keep a Changelog, with one modification: a single **undated** version heading states the shape of the next release,
 * and that is the release line the build stamps.
 *
 * `## [Unreleased]` is optional and carries no build meaning — it is a staging bucket for entries not yet assigned to a
 * version. Dated headings are history. Writing `## [0.6.0-M01]` is therefore all it takes to publish a milestone:
 * nothing in the build needs to understand what `M01` means.
 */
object Changelog {
  // `## [0.6.0-M01] - 2026-07-14`, `## [0.6.0-M01]`, `## 0.6.0-M01`, with or without brackets.
  private val Heading = raw"^##\s+\[?([^\]\s]+)\]?\s*(?:-\s*(\d{4}-\d{2}-\d{2}))?\s*$$".r

  /** Every heading candidate in document order, before filtering to those that are versions. */
  private def candidates(text: String): Seq[ChangelogHeading] =
    text.linesIterator.collect { case Heading(version, date) =>
      ChangelogHeading(version, Option(date))
    }.toSeq

  /** Every version heading, in document order. Non-version headings such as Unreleased are dropped. */
  def headings(text: String): Seq[ChangelogHeading] =
    candidates(text).filter(heading => SemVer.parse(heading.version).isDefined)

  /**
   * The topmost undated version heading.
   *
   * Fails rather than guessing: no undated heading means nobody has declared the next release's shape, and two of them
   * mean it is ambiguous. `source` appears in the message so the reader knows which of several changelogs to open.
   */
  def releaseLine(text: String, source: String): Either[String, String] = {
    val undatedCandidates = candidates(text).filter(_.date.isEmpty).map(_.version).filter(_ != "Unreleased")
    val undatedVersions   = undatedCandidates.filter(candidate => SemVer.parse(candidate).isDefined)

    undatedVersions match {
      case Seq(single) => Right(single)
      case Seq()       =>
        undatedCandidates.headOption match {
          case Some(notAVersion) =>
            Left(s"$source: the undated heading '$notAVersion' is not a semantic version")
          case None =>
            Left(
              s"$source: no undated release heading. Add one naming the next release, for example '## [0.1.0]'"
            )
        }
      case many =>
        Left(s"$source: expected one undated release heading, found ${many.mkString(", ")}")
    }
  }
}

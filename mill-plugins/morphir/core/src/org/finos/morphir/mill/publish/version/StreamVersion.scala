package org.finos.morphir.mill.publish.version

import java.util.Locale

/** Git state for one stream: its nearest matching tag, and where HEAD sits relative to it. */
final case class GitState(lastTag: Option[String], distance: Int, revision: String, dirty: Boolean)

/** What a build is producing. */
enum PublishMode {
  case Release
  case Snapshot(branch: String)
}

/**
 * Composes the version a build publishes: the changelog supplies the release line, git supplies everything after it.
 *
 * The snapshot shapes are the ones this repository already produces — `0.6.0-M01-12-SNAPSHOT` on main and
 * `0.6.0-M01-develop.12.gabc123-SNAPSHOT` elsewhere. Only where the release line comes from has changed, which keeps
 * the behavioural difference to exactly one thing.
 */
object StreamVersion {
  private val Revision = raw"^[0-9a-fA-F]{7,40}$$".r

  def compose(
      releaseLine: String,
      startingVersion: Option[String],
      state: GitState,
      mode: PublishMode,
      stream: TagStream
  ): Either[String, String] =
    for {
      _ <- Either.cond(!state.dirty, (), "working tree is dirty")
      _ <- Either.cond(state.distance >= 0, (), "commit distance must not be negative")
      _ <- Either.cond(
        Revision.matches(state.revision),
        (),
        s"revision '${state.revision}' must be 7 to 40 hexadecimal characters"
      )
      _ <- Either.cond(
        SemVer.parse(releaseLine).isDefined,
        (),
        s"release line '$releaseLine' is not a semantic version"
      )
      _       <- checkFloor(releaseLine, startingVersion)
      version <- mode match {
        case PublishMode.Release          => release(releaseLine, state, stream)
        case PublishMode.Snapshot(branch) => snapshot(releaseLine, state, branch)
      }
    } yield version

  private def checkFloor(releaseLine: String, startingVersion: Option[String]): Either[String, Unit] =
    startingVersion match {
      case None        => Right(())
      case Some(floor) =>
        SemVer.compare(releaseLine, floor).flatMap { ordering =>
          Either.cond(
            ordering >= 0,
            (),
            s"release line '$releaseLine' is below the starting version '$floor'"
          )
        }
    }

  /**
   * A release publishes the release line unchanged, but only when the tag agrees with it. Without that check the tag
   * could stay a human act while still being a way to publish the wrong number.
   */
  private def release(
      releaseLine: String,
      state: GitState,
      stream: TagStream
  ): Either[String, String] = {
    val expected = stream.tagFor(releaseLine)
    state.lastTag match {
      case Some(tag) if tag == expected => Right(releaseLine)
      case Some(tag)                    =>
        Left(s"tag '$tag' does not match the release line '$releaseLine'; expected '$expected'")
      case None =>
        Left(s"a release needs the tag '$expected'; no tag matching ${stream.pattern} was found")
    }
  }

  private def snapshot(releaseLine: String, state: GitState, branch: String): Either[String, String] = {
    val normalized = branch
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^a-z0-9]+", ".")
      .stripPrefix(".")
      .stripSuffix(".")
    if (normalized.isEmpty) Left("publish branch must not be empty")
    else if (normalized == "main") Right(s"$releaseLine-${state.distance}-SNAPSHOT")
    else {
      val revision = state.revision.take(6).toLowerCase(Locale.ROOT)
      Right(s"$releaseLine-$normalized.${state.distance}.g$revision-SNAPSHOT")
    }
  }
}

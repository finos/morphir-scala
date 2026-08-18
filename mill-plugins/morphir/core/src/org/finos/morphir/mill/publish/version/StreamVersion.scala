package org.finos.morphir.mill.publish.version

import java.util.Locale

/** Git state for one stream: its nearest matching tag, and where HEAD sits relative to it. */
final case class GitState(lastTag: Option[String], distance: Int, revision: String, dirty: Boolean)

object GitState {
  implicit val readWriter: upickle.default.ReadWriter[GitState] = upickle.default.macroRW
}

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

  /**
   * Chooses what a build is producing.
   *
   * `MORPHIR_PUBLISH_MODE=snapshot` is explicit and unchanged: it requires `MORPHIR_PUBLISH_BRANCH` and produces
   * exactly that snapshot. Absent or empty, the mode is *inferred* rather than assumed to be a release: a checkout
   * sitting exactly on its stream's tag (distance zero) with that tag agreeing with the changelog's release line is a
   * release; mid-stream, off-tag, or on a tag from another stream's namespace is a snapshot qualified by the given
   * branch. A tag that sits in *this* stream's own namespace at distance zero but disagrees with the changelog is
   * neither: publishing it as a snapshot would let a mistagged release quietly ship to the snapshot repository and
   * report success, so that combination is an error instead — see `release` below for the same guarantee on the
   * explicit-release path. Defaulting an empty environment straight to `Release` would hard-fail every build that is
   * not sitting on a release tag, which is most of them; inferring keeps an empty environment behaviourally close to
   * what `SnapshotVersion.select` did before independent streams existed, where no environment never hard-failed.
   *
   * `branch` is a parameter rather than read here so this stays pure and callers keep full control of where it comes
   * from (git, an env var, whatever the caller's build tool exposes).
   */
  def resolveMode(
      env: Map[String, String],
      state: GitState,
      stream: TagStream,
      releaseLine: String,
      branch: String
  ): Either[String, PublishMode] =
    env.get("MORPHIR_PUBLISH_MODE") match {
      case Some("snapshot") =>
        env.get("MORPHIR_PUBLISH_BRANCH").filter(_.nonEmpty) match {
          case Some(explicitBranch) => Right(PublishMode.Snapshot(explicitBranch))
          case None                 => Left("MORPHIR_PUBLISH_BRANCH is required in snapshot mode")
        }
      case None | Some("") =>
        val onNamespaceTag =
          state.distance == 0 && state.lastTag.exists(tag => stream.versionFromTag(tag).isDefined)
        if (!onNamespaceTag) Right(PublishMode.Snapshot(branch))
        else {
          val tag      = state.lastTag.get
          val expected = stream.tagFor(releaseLine)
          if (tag == expected) Right(PublishMode.Release)
          else Left(s"tag '$tag' does not match the release line '$releaseLine'; expected '$expected'")
        }
      case Some(other) => Left(s"unsupported MORPHIR_PUBLISH_MODE '$other'")
    }

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

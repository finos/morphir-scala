//| mvnDeps: ["com.lihaoyi::mill-libs-util:$MILL_VERSION"]

package millbuild

import mill.util.VcsVersion

import java.util.Locale

object SnapshotVersion {
  private val SemverTag = raw"^v?(\d+\.\d+\.\d+)(?:-([0-9A-Za-z][0-9A-Za-z.-]*))?$$".r
  private val Revision  = raw"^[0-9a-fA-F]{7,40}$$".r

  def format(state: VcsVersion.State, branch: String): Either[String, String] =
    for {
      _ <- Either.cond(state.dirtyHash.isEmpty, (), "working tree is dirty")
      _ <- Either.cond(
        state.commitsSinceLastTag >= 0,
        (),
        "commit distance must not be negative"
      )
      normalizedBranch = branch
        .toLowerCase(Locale.ROOT)
        .replaceAll("[^a-z0-9]+", ".")
        .stripPrefix(".")
        .stripSuffix(".")
      _ <- Either.cond(normalizedBranch.nonEmpty, (), "publish branch must not be empty")
      _ <- Either.cond(
        Revision.matches(state.currentRevision),
        (),
        "revision must be 7 to 40 hexadecimal characters"
      )
      releaseLine <- state.lastTag match {
        case None                                => Right("0.0.0")
        case Some(SemverTag(version, qualifier)) =>
          Right(Option(qualifier).fold(version)(value => s"$version-$value"))
        case Some(tag) => Left(s"nearest tag '$tag' is not a semantic version")
      }
    } yield {
      val revision = state.currentRevision.take(6).toLowerCase(Locale.ROOT)
      s"$releaseLine-$normalizedBranch.${state.commitsSinceLastTag}.g$revision-SNAPSHOT"
    }

  def select(state: VcsVersion.State, env: Map[String, String]): Either[String, String] =
    env.get("MORPHIR_PUBLISH_MODE") match {
      case None | Some("")  => Right(state.format())
      case Some("snapshot") =>
        env.get("MORPHIR_PUBLISH_BRANCH").filter(_.nonEmpty) match {
          case Some(branch) => format(state, branch)
          case None         => Left("MORPHIR_PUBLISH_BRANCH is required in snapshot mode")
        }
      case Some(mode) => Left(s"unsupported MORPHIR_PUBLISH_MODE '$mode'")
    }
}

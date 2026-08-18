package org.finos.morphir.mill.publish.version

import scala.util.control.NonFatal

/**
 * Resolves one stream's git state.
 *
 * Mill's own `VcsVersion` runs `git describe --abbrev=0 --tags` with no `--match`, so it returns the nearest tag of any
 * shape. That is unusable once more than one stream exists: the first `desktop/v0.3.0` tag would become the libraries'
 * nearest tag, and their version derivation would reject it as not a semantic version. Passing the stream's pattern to
 * `--match` is what keeps the streams apart.
 */
object GitStream {
  def resolve(repositoryRoot: os.Path, stream: TagStream): Either[String, GitState] =
    for {
      revision <- run(repositoryRoot, "rev-parse", "HEAD")
      dirty    <- run(repositoryRoot, "status", "--porcelain").map(_.nonEmpty)
      tag      <- nearestTag(repositoryRoot, stream)
      distance <- tag match {
        case Some(value) => run(repositoryRoot, "rev-list", "--count", s"$value..HEAD").map(_.toInt)
        case None        => run(repositoryRoot, "rev-list", "--count", "HEAD").map(_.toInt)
      }
    } yield GitState(tag, distance, revision, dirty)

  /** None rather than an error: an area may legitimately have no release yet. */
  private def nearestTag(repositoryRoot: os.Path, stream: TagStream): Either[String, Option[String]] =
    try {
      val result = os
        .proc("git", "describe", "--abbrev=0", "--tags", "--match", stream.pattern)
        .call(cwd = repositoryRoot, check = false, stdout = os.Pipe, stderr = os.Pipe)
      if (result.exitCode == 0) Right(Some(result.out.text().trim).filter(_.nonEmpty))
      else Right(None)
    } catch { case NonFatal(error) => Left(s"git describe failed: ${error.getMessage}") }

  private def run(repositoryRoot: os.Path, args: String*): Either[String, String] =
    try {
      val result = os.proc("git" +: args).call(cwd = repositoryRoot, check = false, stdout = os.Pipe, stderr = os.Pipe)
      if (result.exitCode == 0) Right(result.out.text().trim)
      else Left(s"git ${args.mkString(" ")} failed (exit ${result.exitCode}): ${result.err.text().trim}")
    } catch { case NonFatal(error) => Left(s"git ${args.mkString(" ")} failed: ${error.getMessage}") }
}

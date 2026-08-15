package morphir.connector.github
package internal

import kyo.*

/** Runs `gh auth token`. Tests inject a fake; live code uses [[PlatformGhAuth]]. */
private[github] trait GhAuth:
  def stdout(args: Chunk[String]): String < (Abort[GithubError] & Async)

private[github] object GhAuth:
  def succeed(out: String): GhAuth =
    new GhAuth:
      def stdout(args: Chunk[String]) =
        val _ = args
        out

  def fail(error: GithubError): GhAuth =
    new GhAuth:
      def stdout(args: Chunk[String]) =
        val _ = args
        Abort.fail(error)

  def platform: GhAuth = PlatformGhAuth

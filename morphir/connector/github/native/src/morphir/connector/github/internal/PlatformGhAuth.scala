package morphir.connector.github
package internal

import kyo.*

/** Scala Native has no process floor for `gh` yet. The public provider exists; lookup fails with Transport. */
private[github] object PlatformGhAuth extends GhAuth:
  private val detail =
    "gh process spawn is not linked on Scala Native"

  def stdout(args: Chunk[String]): String < (Abort[GithubError] & Async) =
    val _ = args
    Abort.fail(GithubError.Transport(detail))

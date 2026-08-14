package morphir.connector.github
package internal

import kyo.*

/**
 * Scala Native does not take `kyo-http` at kyo 1.0.0-RC6. The published `kyo-net` Native artifact was generated on a
 * Linux host: `KqueueBindingsImpl` is a throwing stub (`sys/event.h` unavailable), and `EpollBindingsImpl` still
 * references Linux `epoll` / `eventfd` / `io_uring` symbols that macOS cannot link. OpenSSL flags alone do not fix
 * that. `GithubClient.live` still exists so the public API is the same; listing fails with [[GithubError.Transport]]
 * rather than posting.
 */
private[github] object PlatformLive:

  private val detail =
    "Live GitHub HTTP is not linked on Scala Native at kyo 1.0.0-RC6"

  def make(token: Token): GithubClient =
    val _ = token
    StubClient()

  private final class StubClient() extends GithubClient:
    def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async) =
      Abort.fail(GithubError.Transport(detail))
    def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async) =
      Abort.fail(GithubError.Transport(detail))
    def listDiscussions(repository: RepositoryRef): Chunk[Discussion] < (Abort[GithubError] & Async) =
      Abort.fail(GithubError.Transport(detail))

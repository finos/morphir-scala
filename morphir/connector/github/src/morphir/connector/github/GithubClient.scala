package morphir.connector.github

import kyo.*
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformLive

/**
 * Lists issues, pull requests, and discussions for a repository.
 *
 * [[GithubClient.recorded]] replays GraphQL JSON envelopes. [[GithubClient.fixture]] replays already-decoded values.
 * [[GithubClient.live]] posts to GitHub over `kyo-http` on the JVM and on Node.js. On Scala Native, listing fails with
 * [[GithubError.Transport]] because the published kyo-net Native artifact at 1.0.0-RC6 does not link kqueue on macOS.
 * Tests use recorded or fixture clients and do not call `api.github.com`.
 */
trait GithubClient:
  def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async)
  def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async)
  def listDiscussions(repository: RepositoryRef): Chunk[Discussion] < (Abort[GithubError] & Async)

object GithubClient:

  def fixture(
      issues: Chunk[Issue] = Chunk.empty,
      pullRequests: Chunk[PullRequest] = Chunk.empty,
      discussions: Chunk[Discussion] = Chunk.empty
  ): GithubClient =
    FixtureClient(issues, pullRequests, discussions)

  def recorded(
      issues: String = GraphQl.emptyIssues,
      pullRequests: String = GraphQl.emptyPullRequests,
      discussions: String = GraphQl.emptyDiscussions
  ): GithubClient =
    RecordedClient(issues, pullRequests, discussions)

  def live(token: Token): GithubClient =
    PlatformLive.make(token)

  private[github] def lift[A](result: Result[GithubError, A]): A < Abort[GithubError] =
    result match
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(GithubError.GraphQl(err.getMessage))

  private final class FixtureClient(
      issues: Chunk[Issue],
      pullRequests: Chunk[PullRequest],
      discussions: Chunk[Discussion]
  ) extends GithubClient:
    def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async) =
      issues
    def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async) =
      pullRequests
    def listDiscussions(repository: RepositoryRef): Chunk[Discussion] < (Abort[GithubError] & Async) =
      discussions

  private final class RecordedClient(
      issuesJson: String,
      pullRequestsJson: String,
      discussionsJson: String
  ) extends GithubClient:
    def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssues(issuesJson))
    def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequests(pullRequestsJson))
    def listDiscussions(repository: RepositoryRef): Chunk[Discussion] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussions(discussionsJson))

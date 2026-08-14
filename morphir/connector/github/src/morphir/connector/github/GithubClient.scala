package morphir.connector.github

import kyo.*

/**
 * Lists issues, pull requests, and discussions for a repository.
 *
 * The live GraphQL transport is not wired yet. [[GithubClient.fixture]] is the implementation tests and callers use
 * until `kyo-http` / `caliban-client` are verified on JS and Native.
 */
trait GithubClient:
  def listIssues(repository: RepositoryRef): Result[GithubError, Chunk[Issue]]
  def listPullRequests(repository: RepositoryRef): Result[GithubError, Chunk[PullRequest]]
  def listDiscussions(repository: RepositoryRef): Result[GithubError, Chunk[Discussion]]

object GithubClient:

  def fixture(
      issues: Chunk[Issue] = Chunk.empty,
      pullRequests: Chunk[PullRequest] = Chunk.empty,
      discussions: Chunk[Discussion] = Chunk.empty
  ): GithubClient =
    FixtureClient(issues, pullRequests, discussions)

  private final class FixtureClient(
      issues: Chunk[Issue],
      pullRequests: Chunk[PullRequest],
      discussions: Chunk[Discussion]
  ) extends GithubClient:
    def listIssues(repository: RepositoryRef): Result[GithubError, Chunk[Issue]] =
      Result.succeed(issues)
    def listPullRequests(repository: RepositoryRef): Result[GithubError, Chunk[PullRequest]] =
      Result.succeed(pullRequests)
    def listDiscussions(repository: RepositoryRef): Result[GithubError, Chunk[Discussion]] =
      Result.succeed(discussions)

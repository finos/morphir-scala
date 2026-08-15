package morphir.connector.github

import kyo.*
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformLive

/**
 * Lists issues, pull requests, and discussions for a repository, and looks up one of those objects by number.
 *
 * [[GithubClient.recorded]] replays GraphQL JSON envelopes. [[GithubClient.fixture]] replays already-decoded values.
 * [[GithubClient.live]] posts to GitHub over `kyo-http` on the JVM and on Node.js. Pass a [[Token]] or take
 * [[TokenProvider]] from [[kyo.Env]]. On Scala Native, listing fails with [[GithubError.Transport]] because the
 * published kyo-net Native artifact at 1.0.0-RC6 does not link kqueue on macOS. Tests use recorded or fixture clients
 * and do not call `api.github.com`.
 */
trait GithubClient:
  def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async)
  def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async)
  def listDiscussions(
      repository: RepositoryRef,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Chunk[Discussion] < (Abort[GithubError] & Async)
  def listDiscussionReplies(
      commentId: String,
      after: Maybe[String] = Absent,
      first: Int = 100,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async)
  def getIssue(repository: RepositoryRef, number: Int): Maybe[Issue] < (Abort[GithubError] & Async)
  def getPullRequest(repository: RepositoryRef, number: Int): Maybe[PullRequest] < (Abort[GithubError] & Async)
  def getDiscussion(
      repository: RepositoryRef,
      number: Int,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Maybe[Discussion] < (Abort[GithubError] & Async)

object GithubClient:

  def fixture(
      issues: Chunk[Issue] = Chunk.empty,
      pullRequests: Chunk[PullRequest] = Chunk.empty,
      discussions: Chunk[Discussion] = Chunk.empty,
      discussionReplies: ConnectionPage[DiscussionComment] = ConnectionPage(),
      issue: Maybe[Issue] = Absent,
      pullRequest: Maybe[PullRequest] = Absent,
      discussion: Maybe[Discussion] = Absent
  ): GithubClient =
    FixtureClient(issues, pullRequests, discussions, discussionReplies, issue, pullRequest, discussion)

  def recorded(
      issues: String = GraphQl.emptyIssues,
      pullRequests: String = GraphQl.emptyPullRequests,
      discussions: String = GraphQl.emptyDiscussions,
      discussionReplies: String = GraphQl.emptyDiscussionReplies,
      issue: String = GraphQl.emptyIssue,
      pullRequest: String = GraphQl.emptyPullRequest,
      discussion: String = GraphQl.emptyDiscussion
  ): GithubClient =
    RecordedClient(issues, pullRequests, discussions, discussionReplies, issue, pullRequest, discussion)

  def live(token: Token): GithubClient =
    PlatformLive.make(token)

  def live: GithubClient < (Env[TokenProvider] & Abort[GithubError] & Async) =
    Env.use[TokenProvider](_.token).map(PlatformLive.make)

  private[github] def lift[A](result: Result[GithubError, A]): A < Abort[GithubError] =
    result match
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(GithubError.GraphQl(err.getMessage))

  private final class FixtureClient(
      issues: Chunk[Issue],
      pullRequests: Chunk[PullRequest],
      discussions: Chunk[Discussion],
      discussionReplies: ConnectionPage[DiscussionComment],
      issue: Maybe[Issue],
      pullRequest: Maybe[PullRequest],
      discussion: Maybe[Discussion]
  ) extends GithubClient:
    def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async) =
      issues
    def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async) =
      pullRequests
    def listDiscussions(
        repository: RepositoryRef,
        replyDepth: ReplyDepth
    ): Chunk[Discussion] < (Abort[GithubError] & Async) =
      discussions
    def listDiscussionReplies(
        commentId: String,
        after: Maybe[String],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      discussionReplies
    def getIssue(repository: RepositoryRef, number: Int): Maybe[Issue] < (Abort[GithubError] & Async) =
      issue
    def getPullRequest(repository: RepositoryRef, number: Int): Maybe[PullRequest] < (Abort[GithubError] & Async) =
      pullRequest
    def getDiscussion(
        repository: RepositoryRef,
        number: Int,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GithubError] & Async) =
      discussion

  private final class RecordedClient(
      issuesJson: String,
      pullRequestsJson: String,
      discussionsJson: String,
      discussionRepliesJson: String,
      issueJson: String,
      pullRequestJson: String,
      discussionJson: String
  ) extends GithubClient:
    def listIssues(repository: RepositoryRef): Chunk[Issue] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssues(issuesJson))
    def listPullRequests(repository: RepositoryRef): Chunk[PullRequest] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequests(pullRequestsJson))
    def listDiscussions(
        repository: RepositoryRef,
        replyDepth: ReplyDepth
    ): Chunk[Discussion] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussions(discussionsJson))
    def listDiscussionReplies(
        commentId: String,
        after: Maybe[String],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussionReplies(discussionRepliesJson))
    def getIssue(repository: RepositoryRef, number: Int): Maybe[Issue] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssue(issueJson))
    def getPullRequest(repository: RepositoryRef, number: Int): Maybe[PullRequest] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequest(pullRequestJson))
    def getDiscussion(
        repository: RepositoryRef,
        number: Int,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussion(discussionJson))

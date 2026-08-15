package morphir.connector.github

import kyo.*
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformLive

/**
 * Lists issues, pull requests, and discussions for a repository, and looks up one of those objects by number.
 *
 * List methods return [[ConnectionPage]] and accept `after` / `first` so a caller can page. [[GithubClient.recorded]]
 * replays GraphQL JSON envelopes. [[GithubClient.fixture]] replays already-decoded values. [[GithubClient.live]] posts
 * to GitHub over `kyo-http` on the JVM and on Node.js. Pass a [[Token]] or take [[TokenProvider]] from [[kyo.Env]]. On
 * Scala Native, listing fails with [[GithubError.Transport]] because the published kyo-net Native artifact at 1.0.0-RC6
 * does not link kqueue on macOS. Tests use recorded or fixture clients and do not call `api.github.com`.
 */
trait GithubClient:
  def listIssues(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: Int = 100
  ): ConnectionPage[Issue] < (Abort[GithubError] & Async)
  def listPullRequests(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: Int = 100
  ): ConnectionPage[PullRequest] < (Abort[GithubError] & Async)
  def listDiscussions(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: Int = 100,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[Discussion] < (Abort[GithubError] & Async)
  def listDiscussionReplies(
      commentId: DiscussionCommentId,
      after: Maybe[Cursor] = Absent,
      first: Int = 100,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async)
  def listIssueComments(
      repository: RepositoryRef,
      number: IssueNumber,
      after: Maybe[Cursor] = Absent,
      first: Int = 100
  ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async)
  def listPullRequestComments(
      repository: RepositoryRef,
      number: PullRequestNumber,
      after: Maybe[Cursor] = Absent,
      first: Int = 100
  ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async)
  def listDiscussionComments(
      repository: RepositoryRef,
      number: DiscussionNumber,
      after: Maybe[Cursor] = Absent,
      first: Int = 100,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async)
  def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GithubError] & Async)
  def getPullRequest(
      repository: RepositoryRef,
      number: PullRequestNumber
  ): Maybe[PullRequest] < (Abort[GithubError] & Async)
  def getDiscussion(
      repository: RepositoryRef,
      number: DiscussionNumber,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Maybe[Discussion] < (Abort[GithubError] & Async)

object GithubClient:

  def fixture(
      issues: Chunk[Issue] = Chunk.empty,
      pullRequests: Chunk[PullRequest] = Chunk.empty,
      discussions: Chunk[Discussion] = Chunk.empty,
      discussionReplies: ConnectionPage[DiscussionComment] = ConnectionPage(),
      issueComments: ConnectionPage[IssueComment] = ConnectionPage(),
      pullRequestComments: ConnectionPage[IssueComment] = ConnectionPage(),
      discussionComments: ConnectionPage[DiscussionComment] = ConnectionPage(),
      issue: Maybe[Issue] = Absent,
      pullRequest: Maybe[PullRequest] = Absent,
      discussion: Maybe[Discussion] = Absent
  ): GithubClient =
    FixtureClient(
      issues,
      pullRequests,
      discussions,
      discussionReplies,
      issueComments,
      pullRequestComments,
      discussionComments,
      issue,
      pullRequest,
      discussion
    )

  def recorded(
      issues: String = GraphQl.emptyIssues,
      pullRequests: String = GraphQl.emptyPullRequests,
      discussions: String = GraphQl.emptyDiscussions,
      discussionReplies: String = GraphQl.emptyDiscussionReplies,
      issueComments: String = GraphQl.emptyIssueComments,
      pullRequestComments: String = GraphQl.emptyPullRequestComments,
      discussionComments: String = GraphQl.emptyDiscussionComments,
      issue: String = GraphQl.emptyIssue,
      pullRequest: String = GraphQl.emptyPullRequest,
      discussion: String = GraphQl.emptyDiscussion
  ): GithubClient =
    RecordedClient(
      issues,
      pullRequests,
      discussions,
      discussionReplies,
      issueComments,
      pullRequestComments,
      discussionComments,
      issue,
      pullRequest,
      discussion
    )

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
      issueComments: ConnectionPage[IssueComment],
      pullRequestComments: ConnectionPage[IssueComment],
      discussionComments: ConnectionPage[DiscussionComment],
      issue: Maybe[Issue],
      pullRequest: Maybe[PullRequest],
      discussion: Maybe[Discussion]
  ) extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[Issue] < (Abort[GithubError] & Async) =
      ConnectionPage(nodes = issues)
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[PullRequest] < (Abort[GithubError] & Async) =
      ConnectionPage(nodes = pullRequests)
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GithubError] & Async) =
      ConnectionPage(nodes = discussions)
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      discussionReplies
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      issueComments
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      pullRequestComments
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      discussionComments
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GithubError] & Async) =
      issue
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GithubError] & Async) =
      pullRequest
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GithubError] & Async) =
      discussion

  private final class RecordedClient(
      issuesJson: String,
      pullRequestsJson: String,
      discussionsJson: String,
      discussionRepliesJson: String,
      issueCommentsJson: String,
      pullRequestCommentsJson: String,
      discussionCommentsJson: String,
      issueJson: String,
      pullRequestJson: String,
      discussionJson: String
  ) extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[Issue] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssues(issuesJson))
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[PullRequest] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequests(pullRequestsJson))
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussions(discussionsJson))
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussionReplies(discussionRepliesJson))
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssueComments(issueCommentsJson))
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequestComments(pullRequestCommentsJson))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussionComments(discussionCommentsJson))
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeIssue(issueJson))
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodePullRequest(pullRequestJson))
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GithubError] & Async) =
      lift(GraphQl.decodeDiscussion(discussionJson))

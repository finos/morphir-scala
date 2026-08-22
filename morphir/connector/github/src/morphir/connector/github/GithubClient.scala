package morphir.connector.github

import kyo.*
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformLive

/**
 * Reads repository issues, pull requests, and discussions, plus public and authenticated-viewer gists.
 *
 * List methods return [[ConnectionPage]] and accept `after` / `first` so a caller can page. Gist lists return
 * [[GistSummary]] values; [[getGist]] and [[getMyGist]] load files and the first comment page.
 * [[GithubClient.recorded]] replays GraphQL JSON envelopes. [[GithubClient.fixture]] replays already-decoded values.
 * [[GithubClient.live]] posts to GitHub over `kyo-http` on the JVM and on Node.js. Pass a [[Token]] or take
 * [[TokenProvider]] from [[kyo.Env]]. On Scala Native, reads fail with [[GitHubException.Transport]] because the
 * published kyo-net Native artifact at 1.0.0-RC6 does not link kqueue on macOS. Tests use recorded or fixture clients
 * and do not call `api.github.com`.
 */
trait GithubClient:
  def listIssues(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[Issue] < (Abort[GitHubException] & Async)
  def listPullRequests(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async)
  def listDiscussions(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async)
  def listGists(
      user: GithubLogin,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async)
  def listMyGists(
      privacy: GistPrivacy = GistPrivacy.All,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async)
  def getGist(user: GithubLogin, name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async)
  def getMyGist(name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async)
  def listGistComments(
      user: GithubLogin,
      name: GistName,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[GistComment] < (Abort[GitHubException] & Async)
  def listDiscussionReplies(
      commentId: DiscussionCommentId,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async)
  def listIssueComments(
      repository: RepositoryRef,
      number: IssueNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async)
  def listPullRequestComments(
      repository: RepositoryRef,
      number: PullRequestNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async)
  def listDiscussionComments(
      repository: RepositoryRef,
      number: DiscussionNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async)
  def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GitHubException] & Async)
  def getPullRequest(
      repository: RepositoryRef,
      number: PullRequestNumber
  ): Maybe[PullRequest] < (Abort[GitHubException] & Async)
  def getDiscussion(
      repository: RepositoryRef,
      number: DiscussionNumber,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Maybe[Discussion] < (Abort[GitHubException] & Async)

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
      discussion: Maybe[Discussion] = Absent,
      gists: Chunk[GistSummary] = Chunk.empty,
      myGists: Chunk[GistSummary] = Chunk.empty,
      gist: Maybe[Gist] = Absent,
      myGist: Maybe[Gist] = Absent,
      gistComments: ConnectionPage[GistComment] = ConnectionPage()
  ): GithubClient =
    FixtureClient(
      issues,
      pullRequests,
      discussions,
      gists,
      myGists,
      gist,
      myGist,
      gistComments,
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
      discussion: String = GraphQl.emptyDiscussion,
      gists: String = GraphQl.emptyGists,
      myGists: String = GraphQl.emptyMyGists,
      gist: String = GraphQl.emptyGist,
      myGist: String = GraphQl.emptyMyGist,
      gistComments: String = GraphQl.emptyGistComments
  ): GithubClient =
    RecordedClient(
      issues,
      pullRequests,
      discussions,
      gists,
      myGists,
      gist,
      myGist,
      gistComments,
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

  def live: GithubClient < (Env[TokenProvider] & Abort[GitHubException] & Async) =
    Env.use[TokenProvider](_.token).map(PlatformLive.make)

  private[github] def lift[A](result: Result[GitHubException, A]): A < Abort[GitHubException] =
    result match
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(GitHubException.GraphQl(err.getMessage))

  private final class FixtureClient(
      issues: Chunk[Issue],
      pullRequests: Chunk[PullRequest],
      discussions: Chunk[Discussion],
      gists: Chunk[GistSummary],
      myGists: Chunk[GistSummary],
      gist: Maybe[Gist],
      myGist: Maybe[Gist],
      gistComments: ConnectionPage[GistComment],
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
        first: PageSize
    ): ConnectionPage[Issue] < (Abort[GitHubException] & Async) =
      ConnectionPage(nodes = issues)
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async) =
      ConnectionPage(nodes = pullRequests)
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async) =
      ConnectionPage(nodes = discussions)
    def listGists(
        user: GithubLogin,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      ConnectionPage(nodes = gists)
    def listMyGists(
        privacy: GistPrivacy,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      ConnectionPage(nodes = myGists)
    def getGist(user: GithubLogin, name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      gist
    def getMyGist(name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      myGist
    def listGistComments(
        user: GithubLogin,
        name: GistName,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistComment] < (Abort[GitHubException] & Async) =
      gistComments
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      discussionReplies
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      issueComments
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      pullRequestComments
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      discussionComments
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GitHubException] & Async) =
      issue
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GitHubException] & Async) =
      pullRequest
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GitHubException] & Async) =
      discussion

  private final class RecordedClient(
      issuesJson: String,
      pullRequestsJson: String,
      discussionsJson: String,
      gistsJson: String,
      myGistsJson: String,
      gistJson: String,
      myGistJson: String,
      gistCommentsJson: String,
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
        first: PageSize
    ): ConnectionPage[Issue] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeIssues(issuesJson))
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodePullRequests(pullRequestsJson))
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeDiscussions(discussionsJson))
    def listGists(
        user: GithubLogin,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeGists(gistsJson))
    def listMyGists(
        privacy: GistPrivacy,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeMyGists(myGistsJson))
    def getGist(user: GithubLogin, name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeGist(gistJson))
    def getMyGist(name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeMyGist(myGistJson))
    def listGistComments(
        user: GithubLogin,
        name: GistName,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistComment] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeGistComments(gistCommentsJson))
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeDiscussionReplies(discussionRepliesJson))
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeIssueComments(issueCommentsJson))
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodePullRequestComments(pullRequestCommentsJson))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeDiscussionComments(discussionCommentsJson))
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeIssue(issueJson))
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodePullRequest(pullRequestJson))
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GitHubException] & Async) =
      lift(GraphQl.decodeDiscussion(discussionJson))

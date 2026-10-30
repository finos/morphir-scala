package morphir.connector.github
package internal

import kyo.*

/**
 * Scala Native does not take `kyo-http` at kyo 1.0.0-RC7. The published `kyo-net` Native artifact was generated on a
 * Linux host, so `KqueueBindingsImpl` is a throwing stub (`sys/event.h` unavailable). RC7 fixed the macOS link failure
 * (getkyo/kyo#1896), but a macOS binary would find no I/O backend below that stub. `GithubClient.live` still exists so
 * the public API is the same; listing fails with [[GitHubException.Transport]] rather than posting.
 */
private[github] object PlatformLive:

  private val detail =
    "Live GitHub HTTP is not linked on Scala Native at kyo 1.0.0-RC7"

  def make(token: Token): GithubClient =
    val _ = token
    StubClient()

  def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) =
    val _ = token
    Abort.fail(GitHubException.Transport(detail))

  private final class StubClient() extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[Issue] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listGists(
        user: GithubLogin,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listMyGists(
        privacy: GistPrivacy,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def getGist(user: GithubLogin, name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def getMyGist(name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listGistComments(
        user: GithubLogin,
        name: GistName,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))

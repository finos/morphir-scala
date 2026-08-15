package morphir.connector.github
package internal

import kyo.*

/**
 * Scala Native does not take `kyo-http` at kyo 1.0.0-RC6. The published `kyo-net` Native artifact was generated on a
 * Linux host: `KqueueBindingsImpl` is a throwing stub (`sys/event.h` unavailable), and `EpollBindingsImpl` still
 * references Linux `epoll` / `eventfd` / `io_uring` symbols that macOS cannot link. OpenSSL flags alone do not fix
 * that. `GithubClient.live` still exists so the public API is the same; listing fails with
 * [[GitHubException.Transport]] rather than posting.
 */
private[github] object PlatformLive:

  private val detail =
    "Live GitHub HTTP is not linked on Scala Native at kyo 1.0.0-RC6"

  def make(token: Token): GithubClient =
    val _ = token
    StubClient()

  private final class StubClient() extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[Issue] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      Abort.fail(GitHubException.Transport(detail))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: Int,
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

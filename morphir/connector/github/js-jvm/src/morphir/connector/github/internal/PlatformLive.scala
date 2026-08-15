package morphir.connector.github
package internal

import kyo.*

/** JVM and Node.js live transport: POST GraphQL to api.github.com through kyo-http. */
private[github] object PlatformLive:

  def make(token: Token): GithubClient =
    LiveClient(token)

  private final class LiveClient(token: Token) extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[String],
        first: Int
    ): ConnectionPage[Issue] < (Abort[GithubError] & Async) =
      post[GraphQl.IssuesEnvelope](GraphQl.listIssuesDocument(repository, after, first)).map(env =>
        GithubClient.lift(GraphQl.issuesFrom(env))
      )
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[String],
        first: Int
    ): ConnectionPage[PullRequest] < (Abort[GithubError] & Async) =
      post[GraphQl.PullRequestsEnvelope](GraphQl.listPullRequestsDocument(repository, after, first)).map(env =>
        GithubClient.lift(GraphQl.pullRequestsFrom(env))
      )
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[String],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GithubError] & Async) =
      post[GraphQl.DiscussionsEnvelope](GraphQl.listDiscussionsDocument(repository, after, first, replyDepth)).map(
        env =>
          GithubClient.lift(GraphQl.discussionsFrom(env))
      )
    def listDiscussionReplies(
        commentId: String,
        after: Maybe[String],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      post[GraphQl.NodeRepliesEnvelope](
        GraphQl.listDiscussionRepliesDocument(commentId, after, first, replyDepth)
      ).map(env => GithubClient.lift(GraphQl.discussionRepliesFrom(env)))
    def listIssueComments(
        repository: RepositoryRef,
        number: Int,
        after: Maybe[String],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      post[GraphQl.IssueCommentsEnvelope](GraphQl.listIssueCommentsDocument(repository, number, after, first)).map(
        env => GithubClient.lift(GraphQl.issueCommentsFrom(env))
      )
    def listPullRequestComments(
        repository: RepositoryRef,
        number: Int,
        after: Maybe[String],
        first: Int
    ): ConnectionPage[IssueComment] < (Abort[GithubError] & Async) =
      post[GraphQl.PullRequestCommentsEnvelope](
        GraphQl.listPullRequestCommentsDocument(repository, number, after, first)
      ).map(env => GithubClient.lift(GraphQl.pullRequestCommentsFrom(env)))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: Int,
        after: Maybe[String],
        first: Int,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GithubError] & Async) =
      post[GraphQl.DiscussionCommentsEnvelope](
        GraphQl.listDiscussionCommentsDocument(repository, number, after, first, replyDepth)
      ).map(env => GithubClient.lift(GraphQl.discussionCommentsFrom(env)))
    def getIssue(repository: RepositoryRef, number: Int): Maybe[Issue] < (Abort[GithubError] & Async) =
      post[GraphQl.SingleIssueEnvelope](GraphQl.getIssueDocument(repository, number)).map(env =>
        GithubClient.lift(GraphQl.issueFrom(env))
      )
    def getPullRequest(repository: RepositoryRef, number: Int): Maybe[PullRequest] < (Abort[GithubError] & Async) =
      post[GraphQl.SinglePullRequestEnvelope](GraphQl.getPullRequestDocument(repository, number)).map(env =>
        GithubClient.lift(GraphQl.pullRequestFrom(env))
      )
    def getDiscussion(
        repository: RepositoryRef,
        number: Int,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GithubError] & Async) =
      post[GraphQl.SingleDiscussionEnvelope](GraphQl.getDiscussionDocument(repository, number, replyDepth)).map(env =>
        GithubClient.lift(GraphQl.discussionFrom(env))
      )

    private def post[A: Schema](request: GraphQl.Request): A < (Abort[GithubError] & Async) =
      postBody(request)

    private def post[A: Schema](request: GraphQl.NodeReplyRequest): A < (Abort[GithubError] & Async) =
      postBody(request)

    private def postBody[A: Schema, B: Schema](body: B): A < (Abort[GithubError] & Async) =
      val send =
        HttpClient.withConfig(
          HttpClientConfig()
            .baseUrl("https://api.github.com")
            .filter(HttpFilter.client.bearerAuth(token.unsafeReveal))
            .filter(HttpFilter.client.addHeader("User-Agent", "morphir-connector-github"))
        ) {
          HttpClient.postJson[A]("/graphql", body)
        }
      Abort.run[HttpException](send).map {
        case Result.Success(body) => body
        case Result.Failure(err)  => Abort.fail(mapHttp(err))
        case Result.Panic(err)    => Abort.fail(GithubError.Transport(err.getMessage))
      }

    private def mapHttp(err: HttpException): GithubError =
      err match
        case status: HttpStatusException =>
          status.status.code match
            case 401 => GithubError.Unauthorized(status.getMessage)
            case 403 => GithubError.RateLimited(status.getMessage)
            case _   => GithubError.Transport(status.getMessage)
        case other => GithubError.Transport(other.getMessage)

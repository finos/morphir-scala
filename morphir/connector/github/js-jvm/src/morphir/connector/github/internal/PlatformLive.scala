package morphir.connector.github
package internal

import kyo.*

/** JVM and Node.js live transport: POST GraphQL to api.github.com through kyo-http. */
private[github] object PlatformLive:

  def make(token: Token): GithubClient =
    LiveClient(token)

  def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) =
    postBody[GraphQl.ViewerLoginEnvelope, GraphQl.ViewerLoginRequest](
      token,
      GraphQl.ViewerLoginRequest(GraphQl.viewerLoginDocument),
      mapVerifierHttp,
      _ => GitHubTokenVerifier.httpFailure(0)
    ).map(envelope => GitHubTokenVerifier.lift(GraphQl.viewerLoginFrom(envelope)))

  private final class LiveClient(token: Token) extends GithubClient:
    def listIssues(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[Issue] < (Abort[GitHubException] & Async) =
      post[GraphQl.IssuesEnvelope](GraphQl.listIssuesDocument(repository, after, first)).map(env =>
        GithubClient.lift(GraphQl.issuesFrom(env))
      )
    def listPullRequests(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[PullRequest] < (Abort[GitHubException] & Async) =
      post[GraphQl.PullRequestsEnvelope](GraphQl.listPullRequestsDocument(repository, after, first)).map(env =>
        GithubClient.lift(GraphQl.pullRequestsFrom(env))
      )
    def listDiscussions(
        repository: RepositoryRef,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[Discussion] < (Abort[GitHubException] & Async) =
      post[GraphQl.DiscussionsEnvelope](GraphQl.listDiscussionsDocument(repository, after, first, replyDepth)).map(
        env =>
          GithubClient.lift(GraphQl.discussionsFrom(env))
      )
    def listGists(
        user: GithubLogin,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      post[GraphQl.GistsEnvelope](GraphQl.listGistsDocument(user, after, first)).map(env =>
        GithubClient.lift(GraphQl.gistsFrom(env))
      )
    def listMyGists(
        privacy: GistPrivacy,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistSummary] < (Abort[GitHubException] & Async) =
      post[GraphQl.ViewerGistsEnvelope](GraphQl.listMyGistsDocument(privacy, after, first)).map(env =>
        GithubClient.lift(GraphQl.myGistsFrom(env))
      )
    def getGist(user: GithubLogin, name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      post[GraphQl.SingleGistEnvelope](GraphQl.getGistDocument(user, name)).map(env =>
        GithubClient.lift(GraphQl.gistFrom(env))
      )
    def getMyGist(name: GistName): Maybe[Gist] < (Abort[GitHubException] & Async) =
      post[GraphQl.ViewerSingleGistEnvelope](GraphQl.getMyGistDocument(name)).map(env =>
        GithubClient.lift(GraphQl.myGistFrom(env))
      )
    def listGistComments(
        user: GithubLogin,
        name: GistName,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[GistComment] < (Abort[GitHubException] & Async) =
      post[GraphQl.GistCommentsEnvelope](GraphQl.listGistCommentsDocument(user, name, after, first)).map(env =>
        GithubClient.lift(GraphQl.gistCommentsFrom(env))
      )
    def listDiscussionReplies(
        commentId: DiscussionCommentId,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      post[GraphQl.NodeRepliesEnvelope](
        GraphQl.listDiscussionRepliesDocument(commentId, after, first, replyDepth)
      ).map(env => GithubClient.lift(GraphQl.discussionRepliesFrom(env)))
    def listIssueComments(
        repository: RepositoryRef,
        number: IssueNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      post[GraphQl.IssueCommentsEnvelope](GraphQl.listIssueCommentsDocument(repository, number, after, first)).map(
        env => GithubClient.lift(GraphQl.issueCommentsFrom(env))
      )
    def listPullRequestComments(
        repository: RepositoryRef,
        number: PullRequestNumber,
        after: Maybe[Cursor],
        first: PageSize
    ): ConnectionPage[IssueComment] < (Abort[GitHubException] & Async) =
      post[GraphQl.PullRequestCommentsEnvelope](
        GraphQl.listPullRequestCommentsDocument(repository, number, after, first)
      ).map(env => GithubClient.lift(GraphQl.pullRequestCommentsFrom(env)))
    def listDiscussionComments(
        repository: RepositoryRef,
        number: DiscussionNumber,
        after: Maybe[Cursor],
        first: PageSize,
        replyDepth: ReplyDepth
    ): ConnectionPage[DiscussionComment] < (Abort[GitHubException] & Async) =
      post[GraphQl.DiscussionCommentsEnvelope](
        GraphQl.listDiscussionCommentsDocument(repository, number, after, first, replyDepth)
      ).map(env => GithubClient.lift(GraphQl.discussionCommentsFrom(env)))
    def getIssue(repository: RepositoryRef, number: IssueNumber): Maybe[Issue] < (Abort[GitHubException] & Async) =
      post[GraphQl.SingleIssueEnvelope](GraphQl.getIssueDocument(repository, number)).map(env =>
        GithubClient.lift(GraphQl.issueFrom(env))
      )
    def getPullRequest(
        repository: RepositoryRef,
        number: PullRequestNumber
    ): Maybe[PullRequest] < (Abort[GitHubException] & Async) =
      post[GraphQl.SinglePullRequestEnvelope](GraphQl.getPullRequestDocument(repository, number)).map(env =>
        GithubClient.lift(GraphQl.pullRequestFrom(env))
      )
    def getDiscussion(
        repository: RepositoryRef,
        number: DiscussionNumber,
        replyDepth: ReplyDepth
    ): Maybe[Discussion] < (Abort[GitHubException] & Async) =
      post[GraphQl.SingleDiscussionEnvelope](GraphQl.getDiscussionDocument(repository, number, replyDepth)).map(env =>
        GithubClient.lift(GraphQl.discussionFrom(env))
      )

    private def post[A: Schema](request: GraphQl.Request): A < (Abort[GitHubException] & Async) =
      postBody(request)

    private def post[A: Schema](request: GraphQl.NodeReplyRequest): A < (Abort[GitHubException] & Async) =
      postBody(request)

    private def post[A: Schema](request: GraphQl.UserRequest): A < (Abort[GitHubException] & Async) =
      postBody(request)

    private def post[A: Schema](request: GraphQl.ViewerGistsRequest): A < (Abort[GitHubException] & Async) =
      postBody(request)

    private def post[A: Schema](request: GraphQl.ViewerGistRequest): A < (Abort[GitHubException] & Async) =
      postBody(request)

    private def postBody[A: Schema, B: Schema](body: B): A < (Abort[GitHubException] & Async) =
      PlatformLive.postBody(token, body, mapClientHttp, err => GitHubException.Transport(err.getMessage))

  private def postBody[A: Schema, B: Schema](
      token: Token,
      body: B,
      mapHttp: HttpException => GitHubException,
      mapPanic: Throwable => GitHubException
  ): A < (Abort[GitHubException] & Async) =
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
      case Result.Panic(err)    => Abort.fail(mapPanic(err))
    }

  private def mapVerifierHttp(err: HttpException): GitHubException =
    err match
      case status: HttpStatusException => GitHubTokenVerifier.httpFailure(status.status.code)
      case _                           => GitHubTokenVerifier.httpFailure(0)

  private def mapClientHttp(err: HttpException): GitHubException =
    err match
      case status: HttpStatusException =>
        GitHubException.fromHttpStatus(status.status.code, statusDetail(status))
      case other => GitHubException.Transport(other.getMessage)

  private def statusDetail(status: HttpStatusException): String =
    val message = Option(status.getMessage).map(_.trim).filter(_.nonEmpty)
    message.getOrElse {
      Option(status.body).map(_.toString.trim).filter(_.nonEmpty).getOrElse("")
    }

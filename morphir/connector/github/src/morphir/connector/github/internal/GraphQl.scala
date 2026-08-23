package morphir.connector.github
package internal

import caliban.client.SelectionBuilder
import java.time.Instant
import kyo.*
import kyo.Json.given_Json

/** Wire types, queries, and decoders for the GitHub GraphQL subset. Not part of the published surface. */
private[github] object GraphQl:

  final case class Error(message: String) derives Schema

  final case class Viewer(login: String) derives Schema
  final case class ViewerData(viewer: Maybe[Viewer]) derives Schema
  final case class ViewerLoginEnvelope(data: Maybe[ViewerData], errors: Maybe[Chunk[Error]] = Absent) derives Schema
  final case class ViewerLoginRequest(query: String) derives Schema

  final case class PageInfo(hasNextPage: Boolean = false, endCursor: Maybe[String] = Absent) derives Schema

  final case class Nodes[A](nodes: Chunk[A], pageInfo: Maybe[PageInfo] = Absent) derives Schema

  final case class GistNodes[A](
      nodes: Maybe[Chunk[Maybe[A]]] = Absent,
      pageInfo: Maybe[PageInfo] = Absent
  ) derives Schema

  final case class WireIssueComment(
      author: Maybe[Actor] = Absent,
      body: Maybe[String] = Absent,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent
  ) derives Schema

  final case class WireIssue(
      number: Int,
      title: String,
      body: Maybe[String],
      url: String,
      author: Maybe[Actor] = Absent,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      labels: Maybe[Nodes[Label]] = Absent,
      comments: Maybe[Nodes[WireIssueComment]] = Absent
  ) derives Schema

  final case class IssuesRepository(issues: Nodes[WireIssue]) derives Schema
  final case class IssuesData(repository: Maybe[IssuesRepository]) derives Schema
  final case class IssuesEnvelope(data: Maybe[IssuesData], errors: Maybe[Chunk[Error]] = Absent) derives Schema

  final case class WirePullRequest(
      number: Int,
      title: String,
      body: Maybe[String],
      url: String,
      author: Maybe[Actor] = Absent,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      labels: Maybe[Nodes[Label]] = Absent,
      comments: Maybe[Nodes[WireIssueComment]] = Absent
  ) derives Schema

  final case class PullRequestsRepository(pullRequests: Nodes[WirePullRequest]) derives Schema
  final case class PullRequestsData(repository: Maybe[PullRequestsRepository]) derives Schema
  final case class PullRequestsEnvelope(data: Maybe[PullRequestsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class WireGistSummary(
      name: String,
      description: Maybe[String],
      url: String,
      owner: Maybe[Actor] = Absent,
      isPublic: Boolean = false,
      isFork: Boolean = false,
      stargazerCount: Int = 0,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      pushedAt: Maybe[Instant] = Absent
  ) derives Schema

  final case class GistsUser(gists: GistNodes[WireGistSummary]) derives Schema
  final case class GistsData(user: Maybe[GistsUser]) derives Schema
  final case class GistsEnvelope(data: Maybe[GistsData], errors: Maybe[Chunk[Error]] = Absent) derives Schema
  final case class ViewerGistsData(viewer: Maybe[GistsUser]) derives Schema
  final case class ViewerGistsEnvelope(data: Maybe[ViewerGistsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class WireLanguage(name: String) derives Schema
  final case class WireGistFile(
      name: Maybe[String] = Absent,
      encoding: Maybe[String] = Absent,
      extension: Maybe[String] = Absent,
      language: Maybe[WireLanguage] = Absent,
      size: Maybe[Int] = Absent,
      isImage: Boolean = false,
      isTruncated: Boolean = false,
      text: Maybe[String] = Absent
  ) derives Schema
  final case class WireGistComment(
      author: Maybe[Actor] = Absent,
      body: String,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent
  ) derives Schema
  final case class WireGist(
      name: String,
      description: Maybe[String],
      url: String,
      owner: Maybe[Actor] = Absent,
      isPublic: Boolean = false,
      isFork: Boolean = false,
      stargazerCount: Int = 0,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      pushedAt: Maybe[Instant] = Absent,
      files: Maybe[Chunk[Maybe[WireGistFile]]] = Absent,
      comments: Maybe[GistNodes[WireGistComment]] = Absent
  ) derives Schema
  final case class SingleGistUser(gist: Maybe[WireGist] = Absent) derives Schema
  final case class SingleGistData(user: Maybe[SingleGistUser]) derives Schema
  final case class SingleGistEnvelope(data: Maybe[SingleGistData], errors: Maybe[Chunk[Error]] = Absent) derives Schema
  final case class ViewerSingleGistData(viewer: Maybe[SingleGistUser]) derives Schema
  final case class ViewerSingleGistEnvelope(
      data: Maybe[ViewerSingleGistData],
      errors: Maybe[Chunk[Error]] = Absent
  ) derives Schema
  final case class GistCommentsNode(comments: Maybe[GistNodes[WireGistComment]] = Absent) derives Schema
  final case class GistCommentsUser(gist: Maybe[GistCommentsNode] = Absent) derives Schema
  final case class GistCommentsData(user: Maybe[GistCommentsUser]) derives Schema
  final case class GistCommentsEnvelope(data: Maybe[GistCommentsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class RepositoryVars(owner: String, name: String) derives Schema
  final case class Request(query: String, variables: RepositoryVars) derives Schema

  final case class UserVars(login: String) derives Schema
  final case class UserRequest(query: String, variables: UserVars) derives Schema
  final case class ViewerGistsVars(privacy: String) derives Schema
  final case class ViewerGistsRequest(query: String, variables: ViewerGistsVars) derives Schema
  final case class GistNameVars(name: String) derives Schema
  final case class ViewerGistRequest(query: String, variables: GistNameVars) derives Schema

  final case class NodeReplyVars(id: String, first: Int, after: Maybe[String] = Absent) derives Schema
  final case class NodeReplyRequest(query: String, variables: NodeReplyVars) derives Schema

  final case class NodeReplies(replies: Maybe[Nodes[WireDiscussionComment]] = Absent) derives Schema
  final case class NodeRepliesData(node: Maybe[NodeReplies]) derives Schema
  final case class NodeRepliesEnvelope(data: Maybe[NodeRepliesData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class WireDiscussionComment(
      id: Maybe[String] = Absent,
      author: Maybe[Actor] = Absent,
      body: Maybe[String] = Absent,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      upvoteCount: Int = 0,
      replies: Maybe[Nodes[WireDiscussionComment]] = Absent
  ) derives Schema

  final case class WireDiscussion(
      number: Int,
      title: String,
      body: Maybe[String],
      url: String,
      author: Maybe[Actor] = Absent,
      createdAt: Maybe[Instant] = Absent,
      updatedAt: Maybe[Instant] = Absent,
      upvoteCount: Int = 0,
      labels: Maybe[Nodes[Label]] = Absent,
      answer: Maybe[WireDiscussionComment] = Absent,
      comments: Maybe[Nodes[WireDiscussionComment]] = Absent
  ) derives Schema

  final case class DiscussionsRepository(discussions: Nodes[WireDiscussion]) derives Schema
  final case class DiscussionsData(repository: Maybe[DiscussionsRepository]) derives Schema
  final case class DiscussionsEnvelope(data: Maybe[DiscussionsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class SingleIssueRepository(issue: Maybe[WireIssue] = Absent) derives Schema
  final case class SingleIssueData(repository: Maybe[SingleIssueRepository]) derives Schema
  final case class SingleIssueEnvelope(data: Maybe[SingleIssueData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class SinglePullRequestRepository(pullRequest: Maybe[WirePullRequest] = Absent) derives Schema
  final case class SinglePullRequestData(repository: Maybe[SinglePullRequestRepository]) derives Schema
  final case class SinglePullRequestEnvelope(data: Maybe[SinglePullRequestData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class SingleDiscussionRepository(discussion: Maybe[WireDiscussion] = Absent) derives Schema
  final case class SingleDiscussionData(repository: Maybe[SingleDiscussionRepository]) derives Schema
  final case class SingleDiscussionEnvelope(data: Maybe[SingleDiscussionData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class IssueCommentsNode(comments: Maybe[Nodes[WireIssueComment]] = Absent) derives Schema
  final case class IssueCommentsRepository(issue: Maybe[IssueCommentsNode] = Absent) derives Schema
  final case class IssueCommentsData(repository: Maybe[IssueCommentsRepository]) derives Schema
  final case class IssueCommentsEnvelope(data: Maybe[IssueCommentsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class PullRequestCommentsNode(comments: Maybe[Nodes[WireIssueComment]] = Absent) derives Schema
  final case class PullRequestCommentsRepository(pullRequest: Maybe[PullRequestCommentsNode] = Absent) derives Schema
  final case class PullRequestCommentsData(repository: Maybe[PullRequestCommentsRepository]) derives Schema
  final case class PullRequestCommentsEnvelope(
      data: Maybe[PullRequestCommentsData],
      errors: Maybe[Chunk[Error]] = Absent
  ) derives Schema

  final case class DiscussionCommentsNode(comments: Maybe[Nodes[WireDiscussionComment]] = Absent) derives Schema
  final case class DiscussionCommentsRepository(discussion: Maybe[DiscussionCommentsNode] = Absent) derives Schema
  final case class DiscussionCommentsData(repository: Maybe[DiscussionCommentsRepository]) derives Schema
  final case class DiscussionCommentsEnvelope(
      data: Maybe[DiscussionCommentsData],
      errors: Maybe[Chunk[Error]] = Absent
  ) derives Schema

  val emptyIssues: String            = """{"data":{"repository":{"issues":{"nodes":[]}}}}"""
  val emptyPullRequests: String      = """{"data":{"repository":{"pullRequests":{"nodes":[]}}}}"""
  val emptyDiscussions: String       = """{"data":{"repository":{"discussions":{"nodes":[]}}}}"""
  val emptyGists: String             = """{"data":{"user":{"gists":{"nodes":[]}}}}"""
  val emptyMyGists: String           = """{"data":{"viewer":{"gists":{"nodes":[]}}}}"""
  val emptyGist: String              = """{"data":{"user":{"gist":null}}}"""
  val emptyMyGist: String            = """{"data":{"viewer":{"gist":null}}}"""
  val emptyGistComments: String      = """{"data":{"user":{"gist":{"comments":{"nodes":[]}}}}}"""
  val emptyDiscussionReplies: String =
    """{"data":{"node":{"replies":{"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}}"""
  val emptyIssueComments: String =
    """{"data":{"repository":{"issue":{"comments":{"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}}}"""
  val emptyPullRequestComments: String =
    """{"data":{"repository":{"pullRequest":{"comments":{"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}}}"""
  val emptyDiscussionComments: String =
    """{"data":{"repository":{"discussion":{"comments":{"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}}}"""
  val emptyIssue: String       = """{"data":{"repository":{"issue":null}}}"""
  val emptyPullRequest: String = """{"data":{"repository":{"pullRequest":null}}}"""
  val emptyDiscussion: String  = """{"data":{"repository":{"discussion":null}}}"""

  val viewerLoginDocument: String       = "query MorphirViewerLogin { viewer { login } }"
  private val viewerVerificationFailure = GitHubException.GraphQl("GitHub token verification failed")

  def listIssuesDocument(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): Request =
    queryDocument(
      repository,
      Client.Repository.issues(Some(first.toInt), cursorArg(after))(
        Client.IssueConnection.pageInfo(pageInfoSelection) ~
          Client.IssueConnection.nodes(issueSelection)
      )
    )

  def listPullRequestsDocument(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): Request =
    queryDocument(
      repository,
      Client.Repository.pullRequests(Some(first.toInt), cursorArg(after))(
        Client.PullRequestConnection.pageInfo(pageInfoSelection) ~
          Client.PullRequestConnection.nodes(pullRequestSelection)
      )
    )

  def listDiscussionsDocument(
      repository: RepositoryRef,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Request =
    queryDocument(
      repository,
      Client.Repository.discussions(Some(first.toInt), cursorArg(after))(
        Client.DiscussionConnection.pageInfo(pageInfoSelection) ~
          Client.DiscussionConnection.nodes(discussionSelection(replyDepth.normalized))
      )
    )

  def listGistsDocument(
      user: GithubLogin,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): UserRequest =
    val document = Client.Query.user(user.asString)(
      Client.User.gists(Some(first.toInt), cursorArg(after), Some(Client.GistPrivacy.PUBLIC))(
        Client.GistConnection.pageInfo(pageInfoSelection) ~
          Client.GistConnection.nodes(gistSummarySelection)
      )
    ).toGraphQL()
    UserRequest(document.query, UserVars(user.asString))

  def listMyGistsDocument(
      privacy: GistPrivacy = GistPrivacy.All,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): ViewerGistsRequest =
    val wirePrivacy = privacy match
      case GistPrivacy.All    => Client.GistPrivacy.ALL
      case GistPrivacy.Public => Client.GistPrivacy.PUBLIC
      case GistPrivacy.Secret => Client.GistPrivacy.SECRET
    val document = Client.Query.viewer(
      Client.User.gists(Some(first.toInt), cursorArg(after), Some(wirePrivacy))(
        Client.GistConnection.pageInfo(pageInfoSelection) ~
          Client.GistConnection.nodes(gistSummarySelection)
      )
    ).toGraphQL()
    ViewerGistsRequest(document.query, ViewerGistsVars(wirePrivacy.value))

  def getGistDocument(user: GithubLogin, name: GistName): UserRequest =
    val document = Client.Query.user(user.asString)(
      Client.User.gist(name.asString)(gistSelection)
    ).toGraphQL()
    UserRequest(document.query, UserVars(user.asString))

  def getMyGistDocument(name: GistName): ViewerGistRequest =
    val document = Client.Query.viewer(
      Client.User.gist(name.asString)(gistSelection)
    ).toGraphQL()
    ViewerGistRequest(document.query, GistNameVars(name.asString))

  def listGistCommentsDocument(
      user: GithubLogin,
      name: GistName,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): UserRequest =
    val document = Client.Query.user(user.asString)(
      Client.User.gist(name.asString)(
        Client.Gist.comments(Some(first.toInt), cursorArg(after))(gistCommentsSelection)
      )
    ).toGraphQL()
    UserRequest(document.query, UserVars(user.asString))

  def listDiscussionRepliesDocument(
      commentId: DiscussionCommentId,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): NodeReplyRequest =
    val afterArg   = cursorArg(after)
    val connection =
      Client.DiscussionCommentConnection.pageInfo(
        Client.PageInfo.hasNextPage ~ Client.PageInfo.endCursor
      ) ~ Client.DiscussionCommentConnection.nodes(discussionCommentSelection(replyDepth.normalized))
    val document =
      Client.Query.node(commentId.asString)(
        Client.DiscussionComment.replies(Some(first.toInt), afterArg)(connection)
      ).toGraphQL()
    NodeReplyRequest(document.query, NodeReplyVars(commentId.asString, first.toInt, after.map(_.asString)))

  def getIssueDocument(repository: RepositoryRef, number: IssueNumber): Request =
    queryDocument(repository, Client.Repository.issue(number.toInt)(issueSelection))

  def getPullRequestDocument(repository: RepositoryRef, number: PullRequestNumber): Request =
    queryDocument(repository, Client.Repository.pullRequest(number.toInt)(pullRequestSelection))

  def getDiscussionDocument(
      repository: RepositoryRef,
      number: DiscussionNumber,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Request =
    queryDocument(repository, Client.Repository.discussion(number.toInt)(discussionSelection(replyDepth.normalized)))

  def listIssueCommentsDocument(
      repository: RepositoryRef,
      number: IssueNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): Request =
    queryDocument(
      repository,
      Client.Repository.issue(number.toInt)(
        Client.Issue.comments(Some(first.toInt), cursorArg(after))(issueCommentsSelection)
      )
    )

  def listPullRequestCommentsDocument(
      repository: RepositoryRef,
      number: PullRequestNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default
  ): Request =
    queryDocument(
      repository,
      Client.Repository.pullRequest(number.toInt)(
        Client.PullRequest.comments(Some(first.toInt), cursorArg(after))(issueCommentsSelection)
      )
    )

  def listDiscussionCommentsDocument(
      repository: RepositoryRef,
      number: DiscussionNumber,
      after: Maybe[Cursor] = Absent,
      first: PageSize = PageSize.default,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Request =
    queryDocument(
      repository,
      Client.Repository.discussion(number.toInt)(
        Client.Discussion.comments(Some(first.toInt), cursorArg(after))(
          discussionCommentsSelection(replyDepth.normalized)
        )
      )
    )

  def decodeIssues(json: String): Result[GitHubException, ConnectionPage[Issue]] =
    decodeEnvelopeValue(json, summon[Schema[IssuesEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(repo => page(repo.issues, toIssue)).getOrElse(ConnectionPage())
    }

  def decodeViewerLogin(json: String): Result[GitHubException, GitHubLogin] =
    summon[Schema[ViewerLoginEnvelope]].decodeString(json) match
      case Result.Success(envelope) => viewerLoginFrom(envelope)
      case Result.Failure(_)        => Result.fail(viewerVerificationFailure)
      case Result.Panic(_)          => Result.fail(viewerVerificationFailure)

  def decodePullRequests(json: String): Result[GitHubException, ConnectionPage[PullRequest]] =
    decodeEnvelopeValue(json, summon[Schema[PullRequestsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(
        _.repository
      ).map(repo => page(repo.pullRequests, toPullRequest)).getOrElse(ConnectionPage())
    }

  def decodeDiscussions(json: String): Result[GitHubException, ConnectionPage[Discussion]] =
    decodeEnvelopeValue(json, summon[Schema[DiscussionsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(repo => page(repo.discussions, toDiscussion)).getOrElse(ConnectionPage())
    }

  def decodeGists(json: String): Result[GitHubException, ConnectionPage[GistSummary]] =
    decodeEnvelopeValue(json, summon[Schema[GistsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.user).map(user => page(user.gists, toGistSummary)).getOrElse(ConnectionPage())
    }

  def decodeMyGists(json: String): Result[GitHubException, ConnectionPage[GistSummary]] =
    decodeEnvelopeValue(json, summon[Schema[ViewerGistsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.viewer).map(user => page(user.gists, toGistSummary)).getOrElse(ConnectionPage())
    }

  def decodeGist(json: String): Result[GitHubException, Maybe[Gist]] =
    decodeEnvelopeValue(json, summon[Schema[SingleGistEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.user).flatMap(_.gist).map(toGist)
    }

  def decodeMyGist(json: String): Result[GitHubException, Maybe[Gist]] =
    decodeEnvelopeValue(json, summon[Schema[ViewerSingleGistEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.viewer).flatMap(_.gist).map(toGist)
    }

  def decodeGistComments(json: String): Result[GitHubException, ConnectionPage[GistComment]] =
    decodeEnvelopeValue(json, summon[Schema[GistCommentsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.user).flatMap(_.gist).flatMap(_.comments).map(page(_, toGistComment)).getOrElse(
        ConnectionPage()
      )
    }

  def decodeDiscussionReplies(json: String): Result[GitHubException, ConnectionPage[DiscussionComment]] =
    decodeEnvelopeValue(json, summon[Schema[NodeRepliesEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.node).flatMap(_.replies).map(toConnectionPage).getOrElse(ConnectionPage())
    }

  def decodeIssue(json: String): Result[GitHubException, Maybe[Issue]] =
    decodeEnvelopeValue(json, summon[Schema[SingleIssueEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.issue).map(toIssue)
    }

  def decodePullRequest(json: String): Result[GitHubException, Maybe[PullRequest]] =
    decodeEnvelopeValue(json, summon[Schema[SinglePullRequestEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.pullRequest).map(toPullRequest)
    }

  def decodeDiscussion(json: String): Result[GitHubException, Maybe[Discussion]] =
    decodeEnvelopeValue(json, summon[Schema[SingleDiscussionEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.discussion).map(toDiscussion)
    }

  def decodeIssueComments(json: String): Result[GitHubException, ConnectionPage[IssueComment]] =
    decodeEnvelopeValue(json, summon[Schema[IssueCommentsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.issue).flatMap(_.comments).map(page(_, toIssueComment)).getOrElse(
        ConnectionPage()
      )
    }

  def decodePullRequestComments(json: String): Result[GitHubException, ConnectionPage[IssueComment]] =
    decodeEnvelopeValue(json, summon[Schema[PullRequestCommentsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.pullRequest).flatMap(_.comments).map(
        page(_, toIssueComment)
      ).getOrElse(ConnectionPage())
    }

  def decodeDiscussionComments(json: String): Result[GitHubException, ConnectionPage[DiscussionComment]] =
    decodeEnvelopeValue(json, summon[Schema[DiscussionCommentsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.discussion).flatMap(_.comments).map(toConnectionPage).getOrElse(
        ConnectionPage()
      )
    }

  def issuesFrom(envelope: IssuesEnvelope): Result[GitHubException, ConnectionPage[Issue]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(repo => page(repo.issues, toIssue)).getOrElse(ConnectionPage())
    )

  def viewerLoginFrom(envelope: ViewerLoginEnvelope): Result[GitHubException, GitHubLogin] =
    envelope.errors match
      case Present(errs) if errs.nonEmpty =>
        Result.fail(viewerVerificationFailure)
      case _ =>
        envelope.data.flatMap(_.viewer) match
          case Present(viewer) =>
            GitHubLogin.parse(viewer.login) match
              case Present(login) => Result.succeed(login)
              case Absent         => Result.fail(viewerVerificationFailure)
          case _ => Result.fail(viewerVerificationFailure)

  def pullRequestsFrom(envelope: PullRequestsEnvelope): Result[GitHubException, ConnectionPage[PullRequest]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(
        _.repository
      ).map(repo => page(repo.pullRequests, toPullRequest)).getOrElse(ConnectionPage())
    )

  def discussionsFrom(envelope: DiscussionsEnvelope): Result[GitHubException, ConnectionPage[Discussion]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(repo => page(repo.discussions, toDiscussion)).getOrElse(ConnectionPage())
    )

  def gistsFrom(envelope: GistsEnvelope): Result[GitHubException, ConnectionPage[GistSummary]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.user).map(user => page(user.gists, toGistSummary)).getOrElse(ConnectionPage())
    )

  def myGistsFrom(envelope: ViewerGistsEnvelope): Result[GitHubException, ConnectionPage[GistSummary]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.viewer).map(user => page(user.gists, toGistSummary)).getOrElse(ConnectionPage())
    )

  def gistFrom(envelope: SingleGistEnvelope): Result[GitHubException, Maybe[Gist]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.user).flatMap(_.gist).map(toGist))

  def myGistFrom(envelope: ViewerSingleGistEnvelope): Result[GitHubException, Maybe[Gist]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.viewer).flatMap(_.gist).map(toGist))

  def gistCommentsFrom(envelope: GistCommentsEnvelope): Result[GitHubException, ConnectionPage[GistComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.user).flatMap(_.gist).flatMap(_.comments).map(page(_, toGistComment)).getOrElse(
        ConnectionPage()
      )
    )

  def discussionRepliesFrom(envelope: NodeRepliesEnvelope): Result[GitHubException, ConnectionPage[DiscussionComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.node).flatMap(_.replies).map(toConnectionPage).getOrElse(ConnectionPage())
    )

  def issueFrom(envelope: SingleIssueEnvelope): Result[GitHubException, Maybe[Issue]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.issue).map(toIssue))

  def pullRequestFrom(envelope: SinglePullRequestEnvelope): Result[GitHubException, Maybe[PullRequest]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.pullRequest).map(toPullRequest))

  def discussionFrom(envelope: SingleDiscussionEnvelope): Result[GitHubException, Maybe[Discussion]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.discussion).map(toDiscussion))

  def issueCommentsFrom(envelope: IssueCommentsEnvelope): Result[GitHubException, ConnectionPage[IssueComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).flatMap(_.issue).flatMap(_.comments).map(page(_, toIssueComment)).getOrElse(
        ConnectionPage()
      )
    )

  def pullRequestCommentsFrom(envelope: PullRequestCommentsEnvelope)
      : Result[GitHubException, ConnectionPage[IssueComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).flatMap(_.pullRequest).flatMap(_.comments).map(
        page(_, toIssueComment)
      ).getOrElse(ConnectionPage())
    )

  def discussionCommentsFrom(envelope: DiscussionCommentsEnvelope)
      : Result[GitHubException, ConnectionPage[DiscussionComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).flatMap(_.discussion).flatMap(_.comments).map(toConnectionPage).getOrElse(
        ConnectionPage()
      )
    )

  private val actorSelection = Client.Actor.login ~ Client.Actor.url

  private val repositoryOwnerSelection = Client.RepositoryOwner.login ~ Client.RepositoryOwner.url

  private val pageInfoSelection =
    Client.PageInfo.hasNextPage ~ Client.PageInfo.endCursor

  private val gistSummarySelection =
    Client.Gist.name ~
      Client.Gist.description ~
      Client.Gist.url ~
      Client.Gist.ownerInterface(repositoryOwnerSelection) ~
      Client.Gist.isPublic ~
      Client.Gist.isFork ~
      Client.Gist.stargazerCount ~
      Client.Gist.createdAt ~
      Client.Gist.updatedAt ~
      Client.Gist.pushedAt

  private val gistFileSelection =
    Client.GistFile.name ~
      Client.GistFile.encoding ~
      Client.GistFile.`extension` ~
      Client.GistFile.language(Client.Language.name) ~
      Client.GistFile.size ~
      Client.GistFile.isImage ~
      Client.GistFile.isTruncated ~
      Client.GistFile.text()

  private val gistCommentSelection =
    Client.GistComment.author(actorSelection) ~
      Client.GistComment.body ~
      Client.GistComment.createdAt ~
      Client.GistComment.updatedAt

  private val gistCommentsSelection =
    Client.GistCommentConnection.pageInfo(pageInfoSelection) ~
      Client.GistCommentConnection.nodes(gistCommentSelection)

  private val gistSelection =
    gistSummarySelection ~
      Client.Gist.files(Some(300))(gistFileSelection) ~
      Client.Gist.comments(Some(100))(gistCommentsSelection)

  private val issueCommentSelection =
    Client.IssueComment.author(actorSelection) ~
      Client.IssueComment.body ~
      Client.IssueComment.createdAt ~
      Client.IssueComment.updatedAt

  private val issueCommentsSelection =
    Client.IssueCommentConnection.pageInfo(pageInfoSelection) ~
      Client.IssueCommentConnection.nodes(issueCommentSelection)

  private val discussionReplySelection =
    Client.DiscussionComment.id ~
      Client.DiscussionComment.author(actorSelection) ~
      Client.DiscussionComment.body ~
      Client.DiscussionComment.createdAt ~
      Client.DiscussionComment.updatedAt ~
      Client.DiscussionComment.upvoteCount

  private def discussionCommentSelection(replyDepth: Int): SelectionBuilder[Client.DiscussionComment, Unit] =
    if replyDepth <= 0 then discussionReplySelection.map(_ => ())
    else
      (discussionReplySelection ~
        Client.DiscussionComment.replies(Some(100))(
          Client.DiscussionCommentConnection.pageInfo(
            Client.PageInfo.hasNextPage ~ Client.PageInfo.endCursor
          ) ~ Client.DiscussionCommentConnection.nodes(discussionCommentSelection(replyDepth - 1))
        )).map(_ => ())

  private def discussionCommentsSelection(replyDepth: Int) =
    Client.DiscussionCommentConnection.pageInfo(pageInfoSelection) ~
      Client.DiscussionCommentConnection.nodes(discussionCommentSelection(replyDepth))

  private val issueSelection =
    Client.Issue.number ~
      Client.Issue.title ~
      Client.Issue.body ~
      Client.Issue.url ~
      Client.Issue.author(actorSelection) ~
      Client.Issue.createdAt ~
      Client.Issue.updatedAt ~
      Client.Issue.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.Issue.comments(Some(100))(issueCommentsSelection)

  private val pullRequestSelection =
    Client.PullRequest.number ~
      Client.PullRequest.title ~
      Client.PullRequest.body ~
      Client.PullRequest.url ~
      Client.PullRequest.author(actorSelection) ~
      Client.PullRequest.createdAt ~
      Client.PullRequest.updatedAt ~
      Client.PullRequest.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.PullRequest.comments(Some(100))(issueCommentsSelection)

  private def discussionSelection(replyDepth: Int) =
    Client.Discussion.number ~
      Client.Discussion.title ~
      Client.Discussion.body ~
      Client.Discussion.url ~
      Client.Discussion.author(actorSelection) ~
      Client.Discussion.createdAt ~
      Client.Discussion.updatedAt ~
      Client.Discussion.upvoteCount ~
      Client.Discussion.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.Discussion.answer(discussionCommentSelection(replyDepth)) ~
      Client.Discussion.comments(Some(100))(discussionCommentsSelection(replyDepth))

  private def toIssue(wire: WireIssue): Issue =
    Issue(
      number = IssueNumber.fromWire(wire.number),
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      comments = wire.comments.map(page(_, toIssueComment)).getOrElse(ConnectionPage())
    )

  private def toIssueComment(wire: WireIssueComment): IssueComment =
    IssueComment(
      author = wire.author,
      body = wire.body,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt
    )

  private def toPullRequest(wire: WirePullRequest): PullRequest =
    PullRequest(
      number = PullRequestNumber.fromWire(wire.number),
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      comments = wire.comments.map(page(_, toIssueComment)).getOrElse(ConnectionPage())
    )

  private def toDiscussion(wire: WireDiscussion): Discussion =
    Discussion(
      number = DiscussionNumber.fromWire(wire.number),
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      upvoteCount = wire.upvoteCount,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      answer = wire.answer.map(toDiscussionComment),
      comments = wire.comments.map(toConnectionPage).getOrElse(ConnectionPage())
    )

  private def toDiscussionComment(wire: WireDiscussionComment): DiscussionComment =
    DiscussionComment(
      id = wire.id.flatMap(DiscussionCommentId.parse),
      author = wire.author,
      body = wire.body,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      upvoteCount = wire.upvoteCount,
      replies = wire.replies.map(toConnectionPage).getOrElse(ConnectionPage())
    )

  private def toGistSummary(wire: WireGistSummary): GistSummary =
    GistSummary(
      name = GistName.fromWire(wire.name),
      description = wire.description,
      url = wire.url,
      owner = wire.owner,
      isPublic = wire.isPublic,
      isFork = wire.isFork,
      stargazerCount = wire.stargazerCount,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      pushedAt = wire.pushedAt
    )

  private def toGist(wire: WireGist): Gist =
    Gist(
      summary = GistSummary(
        name = GistName.fromWire(wire.name),
        description = wire.description,
        url = wire.url,
        owner = wire.owner,
        isPublic = wire.isPublic,
        isFork = wire.isFork,
        stargazerCount = wire.stargazerCount,
        createdAt = wire.createdAt,
        updatedAt = wire.updatedAt,
        pushedAt = wire.pushedAt
      ),
      files = wire.files.map(_.flatMap(_.map(toGistFile).toList)).getOrElse(Chunk.empty),
      comments = wire.comments.map(page(_, toGistComment)).getOrElse(ConnectionPage())
    )

  private def toGistFile(wire: WireGistFile): GistFile =
    GistFile(
      name = wire.name,
      encoding = wire.encoding,
      extension = wire.extension,
      language = wire.language.map(_.name),
      size = wire.size,
      isImage = wire.isImage,
      isTruncated = wire.isTruncated,
      text = wire.text
    )

  private def toGistComment(wire: WireGistComment): GistComment =
    GistComment(
      author = wire.author,
      body = wire.body,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt
    )

  private def toConnectionPage(conn: Nodes[WireDiscussionComment]): ConnectionPage[DiscussionComment] =
    page(conn, toDiscussionComment)

  private def page[A, B](conn: Nodes[A], toValue: A => B): ConnectionPage[B] =
    ConnectionPage(
      nodes = conn.nodes.map(toValue),
      hasNextPage = conn.pageInfo.map(_.hasNextPage).getOrElse(false),
      endCursor = conn.pageInfo.flatMap(_.endCursor).flatMap(Cursor.parse)
    )

  private def page[A, B](conn: GistNodes[A], toValue: A => B): ConnectionPage[B] =
    ConnectionPage(
      nodes = conn.nodes.map(_.flatMap(_.map(toValue).toList)).getOrElse(Chunk.empty),
      hasNextPage = conn.pageInfo.map(_.hasNextPage).getOrElse(false),
      endCursor = conn.pageInfo.flatMap(_.endCursor).flatMap(Cursor.parse)
    )

  private def cursorArg(after: Maybe[Cursor]): Option[String] =
    after match
      case Present(cursor) => Some(cursor.asString)
      case Absent          => None

  private def queryDocument[A](
      repository: RepositoryRef,
      inner: SelectionBuilder[Client.Repository, A]
  ): Request =
    val document = Client.Query.repository(repository.owner, repository.name)(inner).toGraphQL()
    Request(document.query, RepositoryVars(repository.owner, repository.name))

  private def fromErrors[A](errors: Maybe[Chunk[Error]], value: A): Result[GitHubException, A] =
    errors match
      case Present(errs) if errs.nonEmpty =>
        Result.fail(GitHubException.GraphQl(errs.map(_.message).mkString("; ")))
      case _ => Result.succeed(value)

  private def decodeEnvelopeValue[A, B](
      json: String,
      schema: Schema[A],
      errorsOf: A => Maybe[Chunk[Error]]
  )(valueOf: A => B): Result[GitHubException, B] =
    schema.decodeString(json) match
      case Result.Success(envelope) => fromErrors(errorsOf(envelope), valueOf(envelope))
      case Result.Failure(err)      => Result.fail(GitHubException.GraphQl(err.getMessage))
      case Result.Panic(err)        => Result.fail(GitHubException.GraphQl(err.getMessage))

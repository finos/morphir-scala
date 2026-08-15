package morphir.connector.github
package internal

import caliban.client.SelectionBuilder
import java.time.Instant
import kyo.*
import kyo.Json.given_Json

/** Wire types, queries, and decoders for the GitHub GraphQL subset. Not part of the published surface. */
private[github] object GraphQl:

  final case class Error(message: String) derives Schema

  final case class PageInfo(hasNextPage: Boolean = false, endCursor: Maybe[String] = Absent) derives Schema

  final case class Nodes[A](nodes: Chunk[A], pageInfo: Maybe[PageInfo] = Absent) derives Schema

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

  final case class RepositoryVars(owner: String, name: String) derives Schema
  final case class Request(query: String, variables: RepositoryVars) derives Schema

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

  val emptyIssues: String            = """{"data":{"repository":{"issues":{"nodes":[]}}}}"""
  val emptyPullRequests: String      = """{"data":{"repository":{"pullRequests":{"nodes":[]}}}}"""
  val emptyDiscussions: String       = """{"data":{"repository":{"discussions":{"nodes":[]}}}}"""
  val emptyDiscussionReplies: String =
    """{"data":{"node":{"replies":{"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}}"""
  val emptyIssue: String       = """{"data":{"repository":{"issue":null}}}"""
  val emptyPullRequest: String = """{"data":{"repository":{"pullRequest":null}}}"""
  val emptyDiscussion: String  = """{"data":{"repository":{"discussion":null}}}"""

  def listIssuesDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.issues(Some(100))(
        Client.IssueConnection.nodes(issueSelection)
      )
    )

  def listPullRequestsDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.pullRequests(Some(100))(
        Client.PullRequestConnection.nodes(pullRequestSelection)
      )
    )

  def listDiscussionsDocument(repository: RepositoryRef, replyDepth: ReplyDepth = ReplyDepth.one): Request =
    queryDocument(
      repository,
      Client.Repository.discussions(Some(100))(
        Client.DiscussionConnection.nodes(discussionSelection(replyDepth.normalized))
      )
    )

  def listDiscussionRepliesDocument(
      commentId: String,
      after: Maybe[String] = Absent,
      first: Int = 100,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): NodeReplyRequest =
    val afterArg = after match
      case Present(cursor) => Some(cursor)
      case Absent          => None
    val connection =
      Client.DiscussionCommentConnection.pageInfo(
        Client.PageInfo.hasNextPage ~ Client.PageInfo.endCursor
      ) ~ Client.DiscussionCommentConnection.nodes(discussionCommentSelection(replyDepth.normalized))
    val document =
      Client.Query.node(commentId)(
        Client.DiscussionComment.replies(Some(first), afterArg)(connection)
      ).toGraphQL()
    NodeReplyRequest(document.query, NodeReplyVars(commentId, first, after))

  def getIssueDocument(repository: RepositoryRef, number: Int): Request =
    queryDocument(repository, Client.Repository.issue(number)(issueSelection))

  def getPullRequestDocument(repository: RepositoryRef, number: Int): Request =
    queryDocument(repository, Client.Repository.pullRequest(number)(pullRequestSelection))

  def getDiscussionDocument(
      repository: RepositoryRef,
      number: Int,
      replyDepth: ReplyDepth = ReplyDepth.one
  ): Request =
    queryDocument(repository, Client.Repository.discussion(number)(discussionSelection(replyDepth.normalized)))

  def decodeIssues(json: String): Result[GithubError, Chunk[Issue]] =
    decodeEnvelope(json, summon[Schema[IssuesEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.issues.nodes.map(toIssue)).getOrElse(Chunk.empty)
    }

  def decodePullRequests(json: String): Result[GithubError, Chunk[PullRequest]] =
    decodeEnvelope(json, summon[Schema[PullRequestsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.pullRequests.nodes.map(toPullRequest)).getOrElse(Chunk.empty)
    }

  def decodeDiscussions(json: String): Result[GithubError, Chunk[Discussion]] =
    decodeEnvelope(json, summon[Schema[DiscussionsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.discussions.nodes.map(toDiscussion)).getOrElse(Chunk.empty)
    }

  def decodeDiscussionReplies(json: String): Result[GithubError, ConnectionPage[DiscussionComment]] =
    decodeEnvelopeValue(json, summon[Schema[NodeRepliesEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.node).flatMap(_.replies).map(toConnectionPage).getOrElse(ConnectionPage())
    }

  def decodeIssue(json: String): Result[GithubError, Maybe[Issue]] =
    decodeEnvelopeValue(json, summon[Schema[SingleIssueEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.issue).map(toIssue)
    }

  def decodePullRequest(json: String): Result[GithubError, Maybe[PullRequest]] =
    decodeEnvelopeValue(json, summon[Schema[SinglePullRequestEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.pullRequest).map(toPullRequest)
    }

  def decodeDiscussion(json: String): Result[GithubError, Maybe[Discussion]] =
    decodeEnvelopeValue(json, summon[Schema[SingleDiscussionEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).flatMap(_.discussion).map(toDiscussion)
    }

  def issuesFrom(envelope: IssuesEnvelope): Result[GithubError, Chunk[Issue]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(_.issues.nodes.map(toIssue)).getOrElse(Chunk.empty)
    )

  def pullRequestsFrom(envelope: PullRequestsEnvelope): Result[GithubError, Chunk[PullRequest]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(_.pullRequests.nodes.map(toPullRequest)).getOrElse(Chunk.empty)
    )

  def discussionsFrom(envelope: DiscussionsEnvelope): Result[GithubError, Chunk[Discussion]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(_.discussions.nodes.map(toDiscussion)).getOrElse(Chunk.empty)
    )

  def discussionRepliesFrom(envelope: NodeRepliesEnvelope): Result[GithubError, ConnectionPage[DiscussionComment]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.node).flatMap(_.replies).map(toConnectionPage).getOrElse(ConnectionPage())
    )

  def issueFrom(envelope: SingleIssueEnvelope): Result[GithubError, Maybe[Issue]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.issue).map(toIssue))

  def pullRequestFrom(envelope: SinglePullRequestEnvelope): Result[GithubError, Maybe[PullRequest]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.pullRequest).map(toPullRequest))

  def discussionFrom(envelope: SingleDiscussionEnvelope): Result[GithubError, Maybe[Discussion]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).flatMap(_.discussion).map(toDiscussion))

  private val actorSelection = Client.Actor.login ~ Client.Actor.url

  private val issueCommentSelection =
    Client.IssueComment.author(actorSelection) ~
      Client.IssueComment.body ~
      Client.IssueComment.createdAt ~
      Client.IssueComment.updatedAt

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

  private val issueSelection =
    Client.Issue.number ~
      Client.Issue.title ~
      Client.Issue.body ~
      Client.Issue.url ~
      Client.Issue.author(actorSelection) ~
      Client.Issue.createdAt ~
      Client.Issue.updatedAt ~
      Client.Issue.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.Issue.comments(Some(100))(Client.IssueCommentConnection.nodes(issueCommentSelection))

  private val pullRequestSelection =
    Client.PullRequest.number ~
      Client.PullRequest.title ~
      Client.PullRequest.body ~
      Client.PullRequest.url ~
      Client.PullRequest.author(actorSelection) ~
      Client.PullRequest.createdAt ~
      Client.PullRequest.updatedAt ~
      Client.PullRequest.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.PullRequest.comments(Some(100))(Client.IssueCommentConnection.nodes(issueCommentSelection))

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
      Client.Discussion.comments(Some(100))(
        Client.DiscussionCommentConnection.nodes(discussionCommentSelection(replyDepth))
      )

  private def toIssue(wire: WireIssue): Issue =
    Issue(
      number = wire.number,
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      comments = wire.comments.map(_.nodes.map(toIssueComment)).getOrElse(Chunk.empty)
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
      number = wire.number,
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      comments = wire.comments.map(_.nodes.map(toIssueComment)).getOrElse(Chunk.empty)
    )

  private def toDiscussion(wire: WireDiscussion): Discussion =
    Discussion(
      number = wire.number,
      title = wire.title,
      body = wire.body,
      url = wire.url,
      author = wire.author,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      upvoteCount = wire.upvoteCount,
      labels = wire.labels.map(_.nodes).getOrElse(Chunk.empty),
      answer = wire.answer.map(toDiscussionComment),
      comments = wire.comments.map(_.nodes.map(toDiscussionComment)).getOrElse(Chunk.empty)
    )

  private def toDiscussionComment(wire: WireDiscussionComment): DiscussionComment =
    DiscussionComment(
      id = wire.id,
      author = wire.author,
      body = wire.body,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      upvoteCount = wire.upvoteCount,
      replies = wire.replies.map(toConnectionPage).getOrElse(ConnectionPage())
    )

  private def toConnectionPage(conn: Nodes[WireDiscussionComment]): ConnectionPage[DiscussionComment] =
    ConnectionPage(
      nodes = conn.nodes.map(toDiscussionComment),
      hasNextPage = conn.pageInfo.map(_.hasNextPage).getOrElse(false),
      endCursor = conn.pageInfo.flatMap(_.endCursor)
    )

  private def queryDocument[A](
      repository: RepositoryRef,
      inner: SelectionBuilder[Client.Repository, A]
  ): Request =
    val document = Client.Query.repository(repository.owner, repository.name)(inner).toGraphQL()
    Request(document.query, RepositoryVars(repository.owner, repository.name))

  private def fromErrors[A](errors: Maybe[Chunk[Error]], value: A): Result[GithubError, A] =
    errors match
      case Present(errs) if errs.nonEmpty =>
        Result.fail(GithubError.GraphQl(errs.map(_.message).mkString("; ")))
      case _ => Result.succeed(value)

  private def decodeEnvelope[A, B](
      json: String,
      schema: Schema[A],
      errorsOf: A => Maybe[Chunk[Error]]
  )(nodesOf: A => Chunk[B]): Result[GithubError, Chunk[B]] =
    decodeEnvelopeValue(json, schema, errorsOf)(nodesOf)

  private def decodeEnvelopeValue[A, B](
      json: String,
      schema: Schema[A],
      errorsOf: A => Maybe[Chunk[Error]]
  )(valueOf: A => B): Result[GithubError, B] =
    schema.decodeString(json) match
      case Result.Success(envelope) => fromErrors(errorsOf(envelope), valueOf(envelope))
      case Result.Failure(err)      => Result.fail(GithubError.GraphQl(err.getMessage))
      case Result.Panic(err)        => Result.fail(GithubError.GraphQl(err.getMessage))

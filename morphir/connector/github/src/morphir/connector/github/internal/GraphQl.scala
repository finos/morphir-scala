package morphir.connector.github
package internal

import caliban.client.SelectionBuilder
import java.time.Instant
import kyo.*
import kyo.Json.given_Json

/** Wire types, queries, and decoders for the GitHub GraphQL subset. Not part of the published surface. */
private[github] object GraphQl:

  final case class Error(message: String) derives Schema

  final case class Nodes[A](nodes: Chunk[A]) derives Schema

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

  final case class WireDiscussionComment(
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

  val emptyIssues: String       = """{"data":{"repository":{"issues":{"nodes":[]}}}}"""
  val emptyPullRequests: String = """{"data":{"repository":{"pullRequests":{"nodes":[]}}}}"""
  val emptyDiscussions: String  = """{"data":{"repository":{"discussions":{"nodes":[]}}}}"""

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

  def listDiscussionsDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.discussions(Some(100))(
        Client.DiscussionConnection.nodes(discussionSelection)
      )
    )

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

  private val actorSelection = Client.Actor.login ~ Client.Actor.url

  private val issueCommentSelection =
    Client.IssueComment.author(actorSelection) ~
      Client.IssueComment.body ~
      Client.IssueComment.createdAt ~
      Client.IssueComment.updatedAt

  private val discussionReplySelection =
    Client.DiscussionComment.author(actorSelection) ~
      Client.DiscussionComment.body ~
      Client.DiscussionComment.createdAt ~
      Client.DiscussionComment.updatedAt ~
      Client.DiscussionComment.upvoteCount

  private val discussionCommentSelection =
    discussionReplySelection ~
      Client.DiscussionComment.replies(Some(100))(
        Client.DiscussionCommentConnection.nodes(discussionReplySelection)
      )

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

  private val discussionSelection =
    Client.Discussion.number ~
      Client.Discussion.title ~
      Client.Discussion.body ~
      Client.Discussion.url ~
      Client.Discussion.author(actorSelection) ~
      Client.Discussion.createdAt ~
      Client.Discussion.updatedAt ~
      Client.Discussion.upvoteCount ~
      Client.Discussion.labels(Some(100))(Client.LabelConnection.nodes(Client.Label.name)) ~
      Client.Discussion.answer(discussionCommentSelection) ~
      Client.Discussion.comments(Some(100))(
        Client.DiscussionCommentConnection.nodes(discussionCommentSelection)
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
      author = wire.author,
      body = wire.body,
      createdAt = wire.createdAt,
      updatedAt = wire.updatedAt,
      upvoteCount = wire.upvoteCount,
      replies = wire.replies.map(_.nodes.map(toDiscussionComment)).getOrElse(Chunk.empty)
    )

  private def queryDocument[A](
      repository: RepositoryRef,
      inner: SelectionBuilder[Client.Repository, A]
  ): Request =
    val document = Client.Query.repository(repository.owner, repository.name)(inner).toGraphQL()
    Request(document.query, RepositoryVars(repository.owner, repository.name))

  private def fromErrors[A](errors: Maybe[Chunk[Error]], nodes: Chunk[A]): Result[GithubError, Chunk[A]] =
    errors match
      case Present(errs) if errs.nonEmpty =>
        Result.fail(GithubError.GraphQl(errs.map(_.message).mkString("; ")))
      case _ => Result.succeed(nodes)

  private def decodeEnvelope[A, B](
      json: String,
      schema: Schema[A],
      errorsOf: A => Maybe[Chunk[Error]]
  )(nodesOf: A => Chunk[B]): Result[GithubError, Chunk[B]] =
    schema.decodeString(json) match
      case Result.Success(envelope) => fromErrors(errorsOf(envelope), nodesOf(envelope))
      case Result.Failure(err)      => Result.fail(GithubError.GraphQl(err.getMessage))
      case Result.Panic(err)        => Result.fail(GithubError.GraphQl(err.getMessage))

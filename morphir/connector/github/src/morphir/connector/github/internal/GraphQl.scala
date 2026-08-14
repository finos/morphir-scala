package morphir.connector.github
package internal

import caliban.client.SelectionBuilder
import kyo.*
import kyo.Json.given_Json

/** Wire types, queries, and decoders for the GitHub GraphQL subset. Not part of the published surface. */
private[github] object GraphQl:

  final case class Error(message: String) derives Schema

  final case class Nodes[A](nodes: Chunk[A]) derives Schema

  final case class IssuesRepository(issues: Nodes[Issue]) derives Schema
  final case class IssuesData(repository: Maybe[IssuesRepository]) derives Schema
  final case class IssuesEnvelope(data: Maybe[IssuesData], errors: Maybe[Chunk[Error]] = Absent) derives Schema

  final case class PullRequestsRepository(pullRequests: Nodes[PullRequest]) derives Schema
  final case class PullRequestsData(repository: Maybe[PullRequestsRepository]) derives Schema
  final case class PullRequestsEnvelope(data: Maybe[PullRequestsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class DiscussionsRepository(discussions: Nodes[Discussion]) derives Schema
  final case class DiscussionsData(repository: Maybe[DiscussionsRepository]) derives Schema
  final case class DiscussionsEnvelope(data: Maybe[DiscussionsData], errors: Maybe[Chunk[Error]] = Absent)
      derives Schema

  final case class RepositoryVars(owner: String, name: String) derives Schema
  final case class Request(query: String, variables: RepositoryVars) derives Schema

  val emptyIssues: String       = """{"data":{"repository":{"issues":{"nodes":[]}}}}"""
  val emptyPullRequests: String = """{"data":{"repository":{"pullRequests":{"nodes":[]}}}}"""
  val emptyDiscussions: String  = """{"data":{"repository":{"discussions":{"nodes":[]}}}}"""

  def listIssuesDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.issues(Some(100))(
        Client.IssueConnection.nodes(
          Client.Issue.number ~ Client.Issue.title ~ Client.Issue.body ~ Client.Issue.url
        )
      )
    )

  def listPullRequestsDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.pullRequests(Some(100))(
        Client.PullRequestConnection.nodes(
          Client.PullRequest.number ~ Client.PullRequest.title ~ Client.PullRequest.body ~ Client.PullRequest.url
        )
      )
    )

  def listDiscussionsDocument(repository: RepositoryRef): Request =
    queryDocument(
      repository,
      Client.Repository.discussions(Some(100))(
        Client.DiscussionConnection.nodes(
          Client.Discussion.number ~ Client.Discussion.title ~ Client.Discussion.body ~ Client.Discussion.url
        )
      )
    )

  def decodeIssues(json: String): Result[GithubError, Chunk[Issue]] =
    decodeEnvelope(json, summon[Schema[IssuesEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.issues.nodes).getOrElse(Chunk.empty)
    }

  def decodePullRequests(json: String): Result[GithubError, Chunk[PullRequest]] =
    decodeEnvelope(json, summon[Schema[PullRequestsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.pullRequests.nodes).getOrElse(Chunk.empty)
    }

  def decodeDiscussions(json: String): Result[GithubError, Chunk[Discussion]] =
    decodeEnvelope(json, summon[Schema[DiscussionsEnvelope]], _.errors) { envelope =>
      envelope.data.flatMap(_.repository).map(_.discussions.nodes).getOrElse(Chunk.empty)
    }

  def issuesFrom(envelope: IssuesEnvelope): Result[GithubError, Chunk[Issue]] =
    fromErrors(envelope.errors, envelope.data.flatMap(_.repository).map(_.issues.nodes).getOrElse(Chunk.empty))

  def pullRequestsFrom(envelope: PullRequestsEnvelope): Result[GithubError, Chunk[PullRequest]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(_.pullRequests.nodes).getOrElse(Chunk.empty)
    )

  def discussionsFrom(envelope: DiscussionsEnvelope): Result[GithubError, Chunk[Discussion]] =
    fromErrors(
      envelope.errors,
      envelope.data.flatMap(_.repository).map(_.discussions.nodes).getOrElse(Chunk.empty)
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

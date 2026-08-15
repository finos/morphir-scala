package morphir.connector.github

import java.time.Instant
import kyo.*

/** How many nested discussion-reply levels to select. Zero omits replies. Negative values are treated as zero. */
final case class ReplyDepth(levels: Int) derives CanEqual:
  def normalized: Int = math.max(0, levels)

object ReplyDepth:
  val none: ReplyDepth = ReplyDepth(0)
  val one: ReplyDepth  = ReplyDepth(1)

/** One page of a GitHub GraphQL connection, including the cursor for the next page. */
final case class ConnectionPage[A](
    nodes: Chunk[A] = Chunk.empty,
    hasNextPage: Boolean = false,
    endCursor: Maybe[String] = Absent
) derives CanEqual, Schema

/** Owner and repository name as GitHub's `repository(owner, name)` arguments. */
final case class RepositoryRef(owner: String, name: String) derives CanEqual

/** A GitHub actor (user, bot, or organization) as GraphQL `Actor.login` and `Actor.url`. */
final case class Actor(login: String, url: String) derives CanEqual, Schema

/** A GitHub label. Field names follow GitHub's GraphQL `Label` type. */
final case class Label(name: String) derives CanEqual, Schema

/** A comment on an issue or pull request. GitHub's `IssueComment` has no `upvoteCount`. */
final case class IssueComment(
    author: Maybe[Actor] = Absent,
    body: Maybe[String] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent
) derives CanEqual, Schema

/** A discussion comment. GitHub's `DiscussionComment` is `Votable` and has nested replies. */
final case class DiscussionComment(
    id: Maybe[String] = Absent,
    author: Maybe[Actor] = Absent,
    body: Maybe[String] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    upvoteCount: Int = 0,
    replies: ConnectionPage[DiscussionComment] = ConnectionPage()
) derives CanEqual, Schema

/** A GitHub issue. Field names follow GitHub's GraphQL `Issue` type, not OKF. */
final case class Issue(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    labels: Chunk[Label] = Chunk.empty,
    comments: Chunk[IssueComment] = Chunk.empty
) derives CanEqual, Schema

/** A GitHub pull request. Field names follow GitHub's GraphQL `PullRequest` type, not OKF. */
final case class PullRequest(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    labels: Chunk[Label] = Chunk.empty,
    comments: Chunk[IssueComment] = Chunk.empty
) derives CanEqual, Schema

/** A GitHub discussion. Field names follow GitHub's GraphQL `Discussion` type, not OKF. */
final case class Discussion(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    upvoteCount: Int = 0,
    labels: Chunk[Label] = Chunk.empty,
    answer: Maybe[DiscussionComment] = Absent,
    comments: Chunk[DiscussionComment] = Chunk.empty
) derives CanEqual, Schema

package morphir.connector.github

import java.time.Instant
import kyo.*
import scala.annotation.targetName

private object GithubOpaqueSchemas:
  val string: Schema[String] = summon[Schema[String]]
  val int: Schema[Int]       = summon[Schema[Int]]

/** How many nested discussion-reply levels to select. Zero omits replies. Negative values are treated as zero. */
final case class ReplyDepth(levels: Int) derives CanEqual:
  def normalized: Int = math.max(0, levels)

object ReplyDepth:
  val none: ReplyDepth = ReplyDepth(0)
  val one: ReplyDepth  = ReplyDepth(1)

/** A GitHub GraphQL connection cursor (`pageInfo.endCursor` / `after`). Not a node id. */
opaque type Cursor = String

object Cursor:
  given CanEqual[Cursor, Cursor] = CanEqual.derived
  given Schema[Cursor]           = GithubOpaqueSchemas.string
  given Render[Cursor]           = Render.from(show)

  def parse(raw: String): Maybe[Cursor] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Absent else Present(trimmed)

  def show(cursor: Cursor): String = s"cursor:${cursor.asString}"

  private[github] def fromWire(raw: String): Cursor = raw

  extension (cursor: Cursor) def asString: String = cursor

/** A positive GitHub GraphQL connection page size (`first`). */
opaque type PageSize = Int

object PageSize:
  given CanEqual[PageSize, PageSize] = CanEqual.derived
  given Schema[PageSize]             = GithubOpaqueSchemas.int

  val default: PageSize = 100

  def parse(n: Int): Maybe[PageSize] =
    if n > 0 then Present(n) else Absent

  private[github] def fromWire(n: Int): PageSize = n

  extension (size: PageSize) def toInt: Int = size

/** Repository issue number (`Issue.number`). Not a discussion number and not a page size. */
opaque type IssueNumber = Int

object IssueNumber:
  given CanEqual[IssueNumber, IssueNumber] = CanEqual.derived
  given Schema[IssueNumber]                = GithubOpaqueSchemas.int
  given Render[IssueNumber]                = Render.from(show)

  def parse(n: Int): Maybe[IssueNumber] =
    if n > 0 then Present(n) else Absent

  def show(n: IssueNumber): String = s"issue:${n.toInt}"

  private[github] def fromWire(n: Int): IssueNumber = n

  extension (n: IssueNumber) def toInt: Int = n

/**
 * Repository pull request number (`PullRequest.number`). GitHub shares issue numbering, but the lookup field is
 * distinct.
 */
opaque type PullRequestNumber = Int

object PullRequestNumber:
  given CanEqual[PullRequestNumber, PullRequestNumber] = CanEqual.derived
  given Schema[PullRequestNumber]                      = GithubOpaqueSchemas.int
  given Render[PullRequestNumber]                      = Render.from(show)

  def parse(n: Int): Maybe[PullRequestNumber] =
    if n > 0 then Present(n) else Absent

  def show(n: PullRequestNumber): String = s"pr:${n.toInt}"

  private[github] def fromWire(n: Int): PullRequestNumber = n

  extension (n: PullRequestNumber) def toInt: Int = n

/** Repository discussion number (`Discussion.number`). Separate from issue and pull request numbers. */
opaque type DiscussionNumber = Int

object DiscussionNumber:
  given CanEqual[DiscussionNumber, DiscussionNumber] = CanEqual.derived
  given Schema[DiscussionNumber]                     = GithubOpaqueSchemas.int
  given Render[DiscussionNumber]                     = Render.from(show)

  def parse(n: Int): Maybe[DiscussionNumber] =
    if n > 0 then Present(n) else Absent

  def show(n: DiscussionNumber): String = s"discussion:${n.toInt}"

  private[github] def fromWire(n: Int): DiscussionNumber = n

  extension (n: DiscussionNumber) def toInt: Int = n

/**
 * Issue and pull request numbers share GitHub's numbering in a repository. The GraphQL lookup field is still distinct,
 * so [[IssueOrPullRequestNumber.fold]] is overloaded on the member type. `@targetName` gives each overload a distinct
 * JVM bytecode name, because both members erase to `Int`.
 */
type IssueOrPullRequestNumber = IssueNumber | PullRequestNumber

object IssueOrPullRequestNumber:
  def toInt(n: IssueOrPullRequestNumber): Int = n

  @targetName("foldIssue")
  def fold[A](number: IssueNumber)(issue: IssueNumber => A, pullRequest: PullRequestNumber => A): A =
    issue(number)

  @targetName("foldPullRequest")
  def fold[A](number: PullRequestNumber)(issue: IssueNumber => A, pullRequest: PullRequestNumber => A): A =
    pullRequest(number)

/**
 * Any repository object number this client looks up. Shared operations such as [[GithubNumber.toInt]] apply to every
 * member. [[GithubNumber.fold]] is overloaded on the member type. `@targetName` gives each overload a distinct JVM
 * bytecode name, because every member erases to `Int`.
 */
type GithubNumber = IssueNumber | PullRequestNumber | DiscussionNumber

object GithubNumber:
  def toInt(n: GithubNumber): Int = n

  @targetName("foldIssue")
  def fold[A](number: IssueNumber)(
      issue: IssueNumber => A,
      pullRequest: PullRequestNumber => A,
      discussion: DiscussionNumber => A
  ): A =
    issue(number)

  @targetName("foldPullRequest")
  def fold[A](number: PullRequestNumber)(
      issue: IssueNumber => A,
      pullRequest: PullRequestNumber => A,
      discussion: DiscussionNumber => A
  ): A =
    pullRequest(number)

  @targetName("foldDiscussion")
  def fold[A](number: DiscussionNumber)(
      issue: IssueNumber => A,
      pullRequest: PullRequestNumber => A,
      discussion: DiscussionNumber => A
  ): A =
    discussion(number)

/** GraphQL global node id of a `DiscussionComment`. Used to page replies; not a connection cursor. */
opaque type DiscussionCommentId = String

object DiscussionCommentId:
  given CanEqual[DiscussionCommentId, DiscussionCommentId] = CanEqual.derived
  given Schema[DiscussionCommentId]                        = GithubOpaqueSchemas.string
  given Render[DiscussionCommentId]                        = Render.from(show)

  def parse(raw: String): Maybe[DiscussionCommentId] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Absent else Present(trimmed)

  def show(id: DiscussionCommentId): String = s"dc:${id.asString}"

  private[github] def fromWire(raw: String): DiscussionCommentId = raw

  extension (id: DiscussionCommentId) def asString: String = id

/** One page of a GitHub GraphQL connection, including the cursor for the next page. */
final case class ConnectionPage[A](
    nodes: Chunk[A] = Chunk.empty,
    hasNextPage: Boolean = false,
    endCursor: Maybe[Cursor] = Absent
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
    id: Maybe[DiscussionCommentId] = Absent,
    author: Maybe[Actor] = Absent,
    body: Maybe[String] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    upvoteCount: Int = 0,
    replies: ConnectionPage[DiscussionComment] = ConnectionPage()
) derives CanEqual, Schema

/** A GitHub issue. Field names follow GitHub's GraphQL `Issue` type, not OKF. */
final case class Issue(
    number: IssueNumber,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    labels: Chunk[Label] = Chunk.empty,
    comments: ConnectionPage[IssueComment] = ConnectionPage()
) derives CanEqual, Schema

/** A GitHub pull request. Field names follow GitHub's GraphQL `PullRequest` type, not OKF. */
final case class PullRequest(
    number: PullRequestNumber,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    labels: Chunk[Label] = Chunk.empty,
    comments: ConnectionPage[IssueComment] = ConnectionPage()
) derives CanEqual, Schema

/** A GitHub discussion. Field names follow GitHub's GraphQL `Discussion` type, not OKF. */
final case class Discussion(
    number: DiscussionNumber,
    title: String,
    body: Maybe[String],
    url: String,
    author: Maybe[Actor] = Absent,
    createdAt: Maybe[Instant] = Absent,
    updatedAt: Maybe[Instant] = Absent,
    upvoteCount: Int = 0,
    labels: Chunk[Label] = Chunk.empty,
    answer: Maybe[DiscussionComment] = Absent,
    comments: ConnectionPage[DiscussionComment] = ConnectionPage()
) derives CanEqual, Schema

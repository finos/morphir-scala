package morphir.knowledge.okf

import kyo.*
import morphir.connector.github.{Discussion, Issue, PullRequest}
import morphir.langkit.markdown.{MD, MdParseError}

/**
 * Maps GitHub-shaped values onto OKF concepts. GitHub types stay in the connector; OKF types stay here.
 *
 * Full ingest (pagination, provenance, round-trip with `kb/`) is later work. This skeleton proves the boundary compiles
 * on JVM, JS, and Native.
 */
object GithubIngest:

  def conceptFromIssue(issue: Issue): Result[MdParseError, Concept] =
    conceptFrom(path = s"issues/${issue.number}.md", title = issue.title, body = issue.body)

  def conceptFromPullRequest(pullRequest: PullRequest): Result[MdParseError, Concept] =
    conceptFrom(path = s"pull-requests/${pullRequest.number}.md", title = pullRequest.title, body = pullRequest.body)

  def conceptFromDiscussion(discussion: Discussion): Result[MdParseError, Concept] =
    conceptFrom(path = s"discussions/${discussion.number}.md", title = discussion.title, body = discussion.body)

  private def conceptFrom(path: String, title: String, body: Maybe[String]): Result[MdParseError, Concept] =
    val source = body.getOrElse("")
    MD.parser.parse(source).map { document =>
      Concept(
        path = path,
        frontmatter = Frontmatter(
          `type` = Present("Concept"),
          title = Present(title),
          description = Present(title)
        ),
        body = document
      )
    }

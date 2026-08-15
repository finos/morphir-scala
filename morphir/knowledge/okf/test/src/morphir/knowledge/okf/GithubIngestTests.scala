package morphir.knowledge.okf

import kyo.*
import kyo.test.*
import morphir.connector.github.{Issue, IssueNumber}

class GithubIngestTests extends Test[Any]:

  private def issueNumber(n: Int): IssueNumber =
    IssueNumber.parse(n) match
      case Present(value) => value
      case Absent         => throw IllegalArgumentException(s"issue number $n")

  "GithubIngest.conceptFromIssue" - {
    "maps number and title onto an OKF concept" in {
      val issue = Issue(issueNumber(7), "A finding", Present("Hello"), "https://example.test/7")
      GithubIngest.conceptFromIssue(issue) match
        case Result.Success(concept) =>
          assert(concept.path == "issues/7.md")
          assert(concept.frontmatter.title == Present("A finding"))
          assert(concept.body.blocks.size == 1)
        case _ => assert(false)
    }
    "parses an absent body as an empty document" in {
      val issue = Issue(issueNumber(8), "Empty", Absent, "https://example.test/8")
      GithubIngest.conceptFromIssue(issue) match
        case Result.Success(concept) => assert(concept.body.blocks.isEmpty)
        case _                       => assert(false)
    }
  }

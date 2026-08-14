package morphir.knowledge.okf

import kyo.*
import kyo.test.*
import morphir.connector.github.Issue

class GithubIngestTests extends Test[Any]:

  "GithubIngest.conceptFromIssue" - {
    "maps number and title onto an OKF concept" in {
      val issue = Issue(7, "A finding", Present("Hello"), "https://example.test/7")
      GithubIngest.conceptFromIssue(issue) match
        case Result.Success(concept) =>
          assert(concept.path == "issues/7.md")
          assert(concept.frontmatter.title == "A finding")
          assert(concept.body.blocks.size == 1)
        case _ => assert(false)
    }
    "parses an absent body as an empty document" in {
      val issue = Issue(8, "Empty", Absent, "https://example.test/8")
      GithubIngest.conceptFromIssue(issue) match
        case Result.Success(concept) => assert(concept.body.blocks.isEmpty)
        case _                       => assert(false)
    }
  }

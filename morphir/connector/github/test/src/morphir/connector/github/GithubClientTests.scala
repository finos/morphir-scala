package morphir.connector.github

import kyo.*
import kyo.test.*

class GithubClientTests extends Test[Any]:

  "Token" - {
    "rejects a blank string" in
      assert(Token.parse("  ").isEmpty)
    "stores a trimmed token" in {
      Token.parse("  ghp_example  ") match
        case Present(token) => assert(token.value == "ghp_example")
        case Absent         => assert(false)
    }
  }

  "GithubClient.fixture" - {
    "returns the recorded issues" in {
      val issue  = Issue(1, "title", Present("body"), "https://example.test/1")
      val client = GithubClient.fixture(issues = Chunk(issue))
      client.listIssues(RepositoryRef("owner", "repo")) match
        case Result.Success(issues) => assert(issues == Chunk(issue))
        case _                      => assert(false)
    }
    "returns empty pull requests and discussions by default" in {
      val client = GithubClient.fixture()
      val repo   = RepositoryRef("owner", "repo")
      assert(client.listPullRequests(repo).isSuccess)
      assert(client.listDiscussions(repo).isSuccess)
      client.listPullRequests(repo) match
        case Result.Success(prs) => assert(prs.isEmpty)
        case _                   => assert(false)
    }
  }

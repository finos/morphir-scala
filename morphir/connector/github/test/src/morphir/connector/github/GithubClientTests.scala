package morphir.connector.github

import kyo.*
import kyo.test.*
import kyo.test.snapshot.*
import morphir.connector.github.internal.GraphQl

class GithubClientTests extends SnapshotTest[Any]:

  override def snapshotDir: String =
    sys.env.getOrElse(
      "MORPHIR_CONNECTOR_GITHUB_SNAPSHOT_DIR",
      "morphir/connector/github/test-snapshots"
    )

  private def run[A](effect: A < (Abort[GithubError] & Async)): Result[GithubError, A] < Async =
    Abort.run[GithubError](effect)

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
      run(client.listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(issues) => assert(issues == Chunk(issue))
        case _                      => assert(false)
      }
    }
    "returns empty pull requests and discussions by default" in {
      val client = GithubClient.fixture()
      val repo   = RepositoryRef("owner", "repo")
      run(client.listPullRequests(repo)).map {
        case Result.Success(prs) => assert(prs.isEmpty)
        case _                   => assert(false)
      }
    }
  }

  "GithubClient.recorded" - {
    "decodes issues from a GraphQL envelope" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      val client = GithubClient.recorded(issues = json)
      run(client.listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(issues) =>
          assert(issues == Chunk(Issue(1, "title", Present("body"), "https://example.test/1")))
        case _ => assert(false)
      }
    }
    "treats a null issue body as Absent" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":2,"title":"untitled","body":null,"url":"https://example.test/2"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(issues) =>
          assert(issues == Chunk(Issue(2, "untitled", Absent, "https://example.test/2")))
        case _ => assert(false)
      }
    }
    "fails with GraphQl when the envelope carries errors" in {
      val json = """{"data":null,"errors":[{"message":"API rate limit exceeded"}]}"""
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Failure(GithubError.GraphQl(detail)) =>
          assert(detail.contains("API rate limit exceeded"))
        case _ => assert(false)
      }
    }
    "decodes pull requests and discussions from GraphQL envelopes" in {
      val prs =
        """{"data":{"repository":{"pullRequests":{"nodes":[{"number":3,"title":"pr","body":"desc","url":"https://example.test/3"}]}}}}"""
      val discussions =
        """{"data":{"repository":{"discussions":{"nodes":[{"number":4,"title":"disc","body":null,"url":"https://example.test/4"}]}}}}"""
      val client = GithubClient.recorded(pullRequests = prs, discussions = discussions)
      val repo   = RepositoryRef("owner", "repo")
      for
        prResult   <- run(client.listPullRequests(repo))
        discResult <- run(client.listDiscussions(repo))
      yield
        prResult match
          case Result.Success(items) =>
            assert(items == Chunk(PullRequest(3, "pr", Present("desc"), "https://example.test/3")))
          case _ => assert(false)
        discResult match
          case Result.Success(items) =>
            assert(items == Chunk(Discussion(4, "disc", Absent, "https://example.test/4")))
          case _ => assert(false)
    }
  }

  "GraphQl.listIssuesDocument" - {
    "matches the blessed list-issues snapshot" in {
      val request = GraphQl.listIssuesDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-issues")
    }
  }

  "GraphQl.listPullRequestsDocument" - {
    "matches the blessed list-pull-requests snapshot" in {
      val request = GraphQl.listPullRequestsDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-pull-requests")
    }
  }

  "GraphQl.listDiscussionsDocument" - {
    "matches the blessed list-discussions snapshot" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-discussions")
    }
  }

  "GithubClient.live" - {
    "is constructible from a token without calling GitHub" in {
      Token.parse("ghp_example") match
        case Present(token) =>
          val _ = GithubClient.live(token)
          assert(true)
        case Absent => assert(false)
    }
  }

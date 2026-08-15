package morphir.connector.github

import kyo.*
import kyo.test.*
import kyo.test.snapshot.*
import morphir.MorphirException
import morphir.appkit.SecretStore
import morphir.connector.github.internal.GhAuth
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformGhAuth

class GithubClientTests extends SnapshotTest[Any]:

  override def snapshotDir: String =
    SnapshotDir.value

  private def run[A](effect: A < (Abort[GitHubException] & Async)): Result[GitHubException, A] < Async =
    Abort.run[GitHubException](effect)

  private def runVault[A](
      store: SecretStore
  )(effect: A < (Env[SecretStore] & Abort[GitHubException] & Async)): Result[GitHubException, A] < Async =
    Abort.run[GitHubException](Env.run(store)(effect))

  private def issueNo(n: Int): IssueNumber              = IssueNumber.fromWire(n)
  private def prNo(n: Int): PullRequestNumber           = PullRequestNumber.fromWire(n)
  private def discNo(n: Int): DiscussionNumber          = DiscussionNumber.fromWire(n)
  private def cursor(s: String): Cursor                 = Cursor.fromWire(s)
  private def commentId(s: String): DiscussionCommentId = DiscussionCommentId.fromWire(s)
  private def pageSize(n: Int): PageSize                = PageSize.fromWire(n)
  private def repo(owner: String, name: String): RepositoryRef =
    RepositoryRef.parse(owner, name) match
      case Present(value) => value
      case Absent         => throw new IllegalArgumentException(s"fixture repo($owner, $name)")

  "GitHubException" - {
    "is catchable as MorphirException" in {
      val err: MorphirException = GitHubException.Unauthorized("missing token")
      val caught                =
        try throw err
        catch case e: MorphirException => e.getMessage == "missing token"
      assert(caught)
    }
    "renders its variant and message" in
      assert(Render.asString(GitHubException.Unauthorized("missing token")) ==
        "GitHubException.Unauthorized: missing token")
    "maps 403 rate-limit wording to RateLimited and other 403s to Forbidden" in {
      assert(
        GitHubException.fromHttpStatus(403, "API rate limit exceeded") ==
          GitHubException.RateLimited("API rate limit exceeded")
      )
      assert(
        GitHubException.fromHttpStatus(403, "Resource not accessible by integration") ==
          GitHubException.Forbidden("Resource not accessible by integration")
      )
      assert(
        GitHubException.fromHttpStatus(401, "Bad credentials") ==
          GitHubException.Unauthorized("Bad credentials")
      )
    }
  }

  "RepositoryRef" - {
    "rejects blank or whitespace-only owner or name" in {
      assert(RepositoryRef.parse("", "repo").isEmpty)
      assert(RepositoryRef.parse("owner", "  ").isEmpty)
      assert(RepositoryRef.parse("  ", "repo").isEmpty)
    }
    "trims owner and name" in
      assert(RepositoryRef.parse("  owner  ", "  repo  ") == Present(repo("owner", "repo")))
  }

  "Token" - {
    "rejects a blank string" in
      assert(Token.parse("  ").isEmpty)
    "stores a trimmed token" in
      assert(Token.parse("  ghp_example  ") == Token.parse("ghp_example"))
    "does not print the secret" in {
      Token.parse("ghp_example") match
        case Present(token) =>
          assert(token.toString == "Token(redacted)")
          assert(!token.toString.contains("ghp_example"))
          assert(token.hashCode == 0)
        case Absent => assert(false)
    }
    "shows prefix and suffix of a long GitHub token" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      Token.parse(secret) match
        case Present(token) =>
          assert(token.toString == "Token(ghp_...abcd)")
          assert(!token.toString.contains("x" * 32))
          assert(!token.toString.contains(secret))
        case Absent => assert(false)
    }
  }

  "PageSize" - {
    "accepts positive sizes and rejects zero or negative sizes" in {
      assert(PageSize.parse(50) == Present(PageSize.fromWire(50)))
      assert(PageSize.parse(0).isEmpty)
      assert(PageSize.parse(-1).isEmpty)
    }
  }

  "IssueNumber" - {
    "rejects zero and negative" in {
      assert(IssueNumber.parse(0).isEmpty)
      assert(IssueNumber.parse(-1).isEmpty)
    }
    "accepts a positive number" in
      assert(IssueNumber.parse(1) == Present(issueNo(1)))
  }

  "PullRequestNumber" - {
    "rejects zero and negative" in {
      assert(PullRequestNumber.parse(0).isEmpty)
      assert(PullRequestNumber.parse(-1).isEmpty)
    }
  }

  "DiscussionNumber" - {
    "rejects zero and negative" in {
      assert(DiscussionNumber.parse(0).isEmpty)
      assert(DiscussionNumber.parse(-1).isEmpty)
    }
  }

  "GithubNumber" - {
    "toInt is shared across members" in {
      val issue: GithubNumber       = issueNo(7)
      val pullRequest: GithubNumber = prNo(3)
      val discussion: GithubNumber  = discNo(4)
      assert(GithubNumber.toInt(issue) == 7)
      assert(GithubNumber.toInt(pullRequest) == 3)
      assert(GithubNumber.toInt(discussion) == 4)
    }
    "fold branches on the member type" in {
      assert(
        GithubNumber.fold(issueNo(1))(issue = _ => "issue", pullRequest = _ => "pr", discussion = _ => "disc") ==
          "issue"
      )
      assert(
        GithubNumber.fold(prNo(1))(issue = _ => "issue", pullRequest = _ => "pr", discussion = _ => "disc") == "pr"
      )
      assert(
        GithubNumber.fold(discNo(1))(issue = _ => "issue", pullRequest = _ => "pr", discussion = _ => "disc") == "disc"
      )
    }
  }

  "IssueOrPullRequestNumber" - {
    "fold branches on the member type" in {
      assert(IssueOrPullRequestNumber.fold(issueNo(1))(issue = _ => "issue", pullRequest = _ => "pr") == "issue")
      assert(IssueOrPullRequestNumber.fold(prNo(1))(issue = _ => "issue", pullRequest = _ => "pr") == "pr")
    }
  }

  "Cursor" - {
    "rejects a blank string" in
      assert(Cursor.parse("  ").isEmpty)
    "stores a trimmed cursor" in
      assert(Cursor.parse("  c1  ") == Cursor.parse("c1"))
  }

  "DiscussionCommentId" - {
    "rejects a blank string" in
      assert(DiscussionCommentId.parse("  ").isEmpty)
    "stores a trimmed id" in
      assert(DiscussionCommentId.parse("  DC_1  ") == DiscussionCommentId.parse("DC_1"))
  }

  "Render" - {
    "names opaque numbers and ids" in {
      assert(Render.asString(issueNo(1)) == "issue:1")
      assert(Render.asString(prNo(3)) == "pr:3")
      assert(Render.asString(discNo(4)) == "discussion:4")
      assert(Render.asString(cursor("c1")) == "cursor:c1")
      assert(Render.asString(commentId("DC_1")) == "dc:DC_1")
    }
    "renders an issue using the opaque number instance" in {
      val issue = Issue(issueNo(1), "title", Present("body"), "https://example.test/1")
      assert(Render.asString(issue).startsWith("Issue(issue:1,"))
    }
    "renders a connection page cursor" in {
      val page = ConnectionPage[Issue](hasNextPage = true, endCursor = Present(cursor("c2")))
      assert(Render.asString(page).contains("cursor:c2"))
    }
  }

  "GithubClient.fixture" - {
    "returns the recorded issues" in {
      val issue  = Issue(issueNo(1), "title", Present("body"), "https://example.test/1")
      val client = GithubClient.fixture(issues = Chunk(issue))
      run(client.listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) => assert(page.nodes == Chunk(issue))
        case _                    => assert(false)
      }
    }
    "returns empty pull requests and discussions by default" in {
      val client = GithubClient.fixture()
      val repository   = repo("owner", "repo")
      run(client.listPullRequests(repository)).map {
        case Result.Success(page) => assert(page.nodes.isEmpty)
        case _                    => assert(false)
      }
    }
    "folds comment paging onto the issue or pull request field" in {
      val issueComments = ConnectionPage(nodes = Chunk(IssueComment(body = Present("issue"))))
      val prComments    = ConnectionPage(nodes = Chunk(IssueComment(body = Present("pr"))))
      val client        = GithubClient.fixture(issueComments = issueComments, pullRequestComments = prComments)
      val repository          = repo("owner", "repo")
      for
        issueResult <- run(
          IssueOrPullRequestNumber.fold(issueNo(1))(
            issue = n => client.listIssueComments(repository, n),
            pullRequest = n => client.listPullRequestComments(repository, n)
          )
        )
        prResult <- run(
          IssueOrPullRequestNumber.fold(prNo(3))(
            issue = n => client.listIssueComments(repository, n),
            pullRequest = n => client.listPullRequestComments(repository, n)
          )
        )
      yield
        issueResult match
          case Result.Success(page) => assert(page == issueComments)
          case _                    => assert(false)
        prResult match
          case Result.Success(page) => assert(page == prComments)
          case _                    => assert(false)
    }
  }

  "GithubClient.recorded" - {
    "decodes issues from a GraphQL envelope" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      val client = GithubClient.recorded(issues = json)
      run(client.listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.nodes == Chunk(Issue(issueNo(1), "title", Present("body"), "https://example.test/1")))
        case _ => assert(false)
      }
    }
    "decodes issue pageInfo from a GraphQL envelope" in {
      val json =
        """{"data":{"repository":{"issues":{"pageInfo":{"hasNextPage":true,"endCursor":"c2"},"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present(cursor("c2")))
          assert(page.nodes == Chunk(Issue(issueNo(1), "title", Present("body"), "https://example.test/1")))
        case _ => assert(false)
      }
    }
    "treats missing issue pageInfo as the last page" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(!page.hasNextPage)
          assert(page.endCursor == Absent)
        case _ => assert(false)
      }
    }
    "treats a null issue body as Absent" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":2,"title":"untitled","body":null,"url":"https://example.test/2"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.nodes == Chunk(Issue(issueNo(2), "untitled", Absent, "https://example.test/2")))
        case _ => assert(false)
      }
    }
    "decodes issue author, dates, labels, and comments" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{
          |"number":1,"title":"title","body":"body","url":"https://example.test/1",
          |"author":{"login":"ada","url":"https://github.com/ada"},
          |"createdAt":"2026-01-02T03:04:05Z","updatedAt":"2026-01-03T04:05:06Z",
          |"labels":{"nodes":[{"name":"bug"}]},
          |"comments":{"nodes":[{
          |"author":{"login":"bob","url":"https://github.com/bob"},
          |"body":"repro?",
          |"createdAt":"2026-01-02T05:00:00Z","updatedAt":"2026-01-02T05:00:00Z"
          }]}
        }]}}}}""".stripMargin.replaceAll("\n", "")
      val ada = Present(Actor("ada", "https://github.com/ada"))
      val bob = Present(Actor("bob", "https://github.com/bob"))
      run(GithubClient.recorded(issues = json).listIssues(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Issue(
                number = issueNo(1),
                title = "title",
                body = Present("body"),
                url = "https://example.test/1",
                author = ada,
                createdAt = Present(java.time.Instant.parse("2026-01-02T03:04:05Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-03T04:05:06Z")),
                labels = Chunk(Label("bug")),
                comments = ConnectionPage(
                  nodes = Chunk(
                    IssueComment(
                      author = bob,
                      body = Present("repro?"),
                      createdAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z")),
                      updatedAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z"))
                    )
                  )
                )
              )
            )
          )
        case _ => assert(false)
      }
    }
    "fails with GraphQl when the envelope carries errors" in {
      val json = """{"data":null,"errors":[{"message":"API rate limit exceeded"}]}"""
      run(GithubClient.recorded(issues = json).listIssues(repo("owner", "repo"))).map {
        case Result.Failure(GitHubException.GraphQl(detail)) =>
          assert(detail.contains("API rate limit exceeded"))
        case _ => assert(false)
      }
    }
    "decodes pull request author, dates, labels, and comments" in {
      val json =
        """{"data":{"repository":{"pullRequests":{"nodes":[{
          |"number":3,"title":"pr","body":"desc","url":"https://example.test/3",
          |"author":{"login":"ada","url":"https://github.com/ada"},
          |"createdAt":"2026-01-02T03:04:05Z","updatedAt":"2026-01-03T04:05:06Z",
          |"labels":{"nodes":[{"name":"enhancement"}]},
          |"comments":{"nodes":[{
          |"author":{"login":"bob","url":"https://github.com/bob"},
          |"body":"lgtm",
          |"createdAt":"2026-01-02T05:00:00Z","updatedAt":"2026-01-02T05:00:00Z"
          }]}
        }]}}}}""".stripMargin.replaceAll("\n", "")
      val ada = Present(Actor("ada", "https://github.com/ada"))
      val bob = Present(Actor("bob", "https://github.com/bob"))
      run(GithubClient.recorded(pullRequests = json).listPullRequests(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              PullRequest(
                number = prNo(3),
                title = "pr",
                body = Present("desc"),
                url = "https://example.test/3",
                author = ada,
                createdAt = Present(java.time.Instant.parse("2026-01-02T03:04:05Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-03T04:05:06Z")),
                labels = Chunk(Label("enhancement")),
                comments = ConnectionPage(
                  nodes = Chunk(
                    IssueComment(
                      author = bob,
                      body = Present("lgtm"),
                      createdAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z")),
                      updatedAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z"))
                    )
                  )
                )
              )
            )
          )
        case _ => assert(false)
      }
    }
    "decodes pull requests and discussions from GraphQL envelopes" in {
      val prs =
        """{"data":{"repository":{"pullRequests":{"nodes":[{"number":3,"title":"pr","body":"desc","url":"https://example.test/3"}]}}}}"""
      val discussions =
        """{"data":{"repository":{"discussions":{"nodes":[{"number":4,"title":"disc","body":null,"url":"https://example.test/4"}]}}}}"""
      val client = GithubClient.recorded(pullRequests = prs, discussions = discussions)
      val repository   = repo("owner", "repo")
      for
        prResult   <- run(client.listPullRequests(repository))
        discResult <- run(client.listDiscussions(repository))
      yield
        prResult match
          case Result.Success(page) =>
            assert(page.nodes == Chunk(PullRequest(prNo(3), "pr", Present("desc"), "https://example.test/3")))
          case _ => assert(false)
        discResult match
          case Result.Success(page) =>
            assert(page.nodes == Chunk(Discussion(discNo(4), "disc", Absent, "https://example.test/4")))
          case _ => assert(false)
    }
    "decodes who posted a discussion" in {
      val json =
        """{"data":{"repository":{"discussions":{"nodes":[{"number":4,"title":"disc","body":null,"url":"https://example.test/4","author":{"login":"ada","url":"https://github.com/ada"}}]}}}}"""
      run(GithubClient.recorded(discussions = json).listDiscussions(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Discussion(
                discNo(4),
                "disc",
                Absent,
                "https://example.test/4",
                Present(Actor("ada", "https://github.com/ada"))
              )
            )
          )
        case _ => assert(false)
      }
    }
    "decodes discussion dates, labels, comments, and upvotes" in {
      val json =
        """{"data":{"repository":{"discussions":{"nodes":[{
          |"number":4,"title":"disc","body":null,"url":"https://example.test/4",
          |"author":{"login":"ada","url":"https://github.com/ada"},
          |"createdAt":"2026-01-02T03:04:05Z","updatedAt":"2026-01-03T04:05:06Z",
          |"upvoteCount":7,
          |"labels":{"nodes":[{"name":"q-and-a"}]},
          |"answer":{"author":{"login":"bob","url":"https://github.com/bob"},"body":"try this","createdAt":"2026-01-02T05:00:00Z","updatedAt":"2026-01-02T05:00:00Z","upvoteCount":4,"replies":{"nodes":[]}},
          |"comments":{"nodes":[{
          |"author":{"login":"bob","url":"https://github.com/bob"},
          |"body":"try this",
          |"createdAt":"2026-01-02T05:00:00Z","updatedAt":"2026-01-02T05:00:00Z",
          |"upvoteCount":4,
          |"replies":{"nodes":[{
          |"author":{"login":"ada","url":"https://github.com/ada"},
          |"body":"thanks",
          |"createdAt":"2026-01-02T06:00:00Z","updatedAt":"2026-01-02T06:00:00Z",
          |"upvoteCount":1
            }]}
          }]}
        }]}}}}""".stripMargin.replaceAll("\n", "")
      val ada   = Present(Actor("ada", "https://github.com/ada"))
      val bob   = Present(Actor("bob", "https://github.com/bob"))
      val reply = DiscussionComment(
        author = ada,
        body = Present("thanks"),
        createdAt = Present(java.time.Instant.parse("2026-01-02T06:00:00Z")),
        updatedAt = Present(java.time.Instant.parse("2026-01-02T06:00:00Z")),
        upvoteCount = 1
      )
      val comment = DiscussionComment(
        author = bob,
        body = Present("try this"),
        createdAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z")),
        updatedAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z")),
        upvoteCount = 4,
        replies = ConnectionPage(nodes = Chunk(reply))
      )
      run(GithubClient.recorded(discussions = json).listDiscussions(repo("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Discussion(
                number = discNo(4),
                title = "disc",
                body = Absent,
                url = "https://example.test/4",
                author = ada,
                createdAt = Present(java.time.Instant.parse("2026-01-02T03:04:05Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-03T04:05:06Z")),
                upvoteCount = 7,
                labels = Chunk(Label("q-and-a")),
                answer = Present(comment.copy(replies = ConnectionPage())),
                comments = ConnectionPage(nodes = Chunk(comment))
              )
            )
          )
        case _ => assert(false)
      }
    }
    "decodes a single issue by number" in {
      val json =
        """{"data":{"repository":{"issue":{"number":1,"title":"title","body":"body","url":"https://example.test/1"}}}}"""
      run(GithubClient.recorded(issue = json).getIssue(repo("owner", "repo"), issueNo(1))).map {
        case Result.Success(Present(issue)) =>
          assert(issue == Issue(issueNo(1), "title", Present("body"), "https://example.test/1"))
        case _ => assert(false)
      }
    }
    "treats a missing issue as Absent" in {
      val json = """{"data":{"repository":{"issue":null}}}"""
      run(GithubClient.recorded(issue = json).getIssue(repo("owner", "repo"), issueNo(99))).map {
        case Result.Success(Absent) => assert(true)
        case _                      => assert(false)
      }
    }
    "decodes a single pull request by number" in {
      val json =
        """{"data":{"repository":{"pullRequest":{"number":3,"title":"pr","body":"desc","url":"https://example.test/3"}}}}"""
      run(GithubClient.recorded(pullRequest = json).getPullRequest(repo("owner", "repo"), prNo(3))).map {
        case Result.Success(Present(pr)) =>
          assert(pr == PullRequest(prNo(3), "pr", Present("desc"), "https://example.test/3"))
        case _ => assert(false)
      }
    }
    "decodes a single discussion by number" in {
      val json =
        """{"data":{"repository":{"discussion":{"number":4,"title":"disc","body":null,"url":"https://example.test/4"}}}}"""
      run(GithubClient.recorded(discussion = json).getDiscussion(repo("owner", "repo"), discNo(4))).map {
        case Result.Success(Present(item)) =>
          assert(item == Discussion(discNo(4), "disc", Absent, "https://example.test/4"))
        case _ => assert(false)
      }
    }
    "decodes a page of additional discussion replies" in {
      val json =
        """{"data":{"node":{"replies":{"pageInfo":{"hasNextPage":true,"endCursor":"cursor-2"},"nodes":[{
          |"id":"DC_2","author":{"login":"ada","url":"https://github.com/ada"},
          |"body":"thanks","createdAt":"2026-01-02T06:00:00Z","updatedAt":"2026-01-02T06:00:00Z","upvoteCount":1
        }]}}}}""".stripMargin.replaceAll("\n", "")
      run(
        GithubClient.recorded(discussionReplies =
          json
        ).listDiscussionReplies(commentId("DC_1"), after = Present(cursor("cursor-1")))
      ).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present(cursor("cursor-2")))
          assert(
            page.nodes == Chunk(
              DiscussionComment(
                id = Present(commentId("DC_2")),
                author = Present(Actor("ada", "https://github.com/ada")),
                body = Present("thanks"),
                createdAt = Present(java.time.Instant.parse("2026-01-02T06:00:00Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-02T06:00:00Z")),
                upvoteCount = 1
              )
            )
          )
        case _ => assert(false)
      }
    }
    "decodes a page of additional issue comments" in {
      val json =
        """{"data":{"repository":{"issue":{"comments":{"pageInfo":{"hasNextPage":true,"endCursor":"c2"},"nodes":[{
          |"author":{"login":"bob","url":"https://github.com/bob"},
          |"body":"repro?","createdAt":"2026-01-02T05:00:00Z","updatedAt":"2026-01-02T05:00:00Z"
        }]}}}}}""".stripMargin.replaceAll("\n", "")
      run(
        GithubClient.recorded(issueComments = json).listIssueComments(
          repo("owner", "repo"),
          issueNo(1),
          after = Present(cursor("c1"))
        )
      ).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present(cursor("c2")))
          assert(
            page.nodes == Chunk(
              IssueComment(
                author = Present(Actor("bob", "https://github.com/bob")),
                body = Present("repro?"),
                createdAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-02T05:00:00Z"))
              )
            )
          )
        case _ => assert(false)
      }
    }
  }

  "GraphQl.listIssuesDocument" - {
    "matches the blessed list-issues snapshot" in {
      val request = GraphQl.listIssuesDocument(repo("acme", "widgets"))
      assertSnapshot(request.query, "list-issues")
    }
    "passes after and first into the issues query" in {
      val request =
        GraphQl.listIssuesDocument(
          repo("acme", "widgets"),
          after = Present(cursor("cursor1")),
          first = pageSize(50)
        )
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listPullRequestsDocument" - {
    "matches the blessed list-pull-requests snapshot" in {
      val request = GraphQl.listPullRequestsDocument(repo("acme", "widgets"))
      assertSnapshot(request.query, "list-pull-requests")
    }
    "passes after and first into the pull requests query" in {
      val request =
        GraphQl.listPullRequestsDocument(
          repo("acme", "widgets"),
          after = Present(cursor("cursor1")),
          first = pageSize(50)
        )
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listDiscussionsDocument" - {
    "matches the blessed list-discussions snapshot" in {
      val request = GraphQl.listDiscussionsDocument(repo("acme", "widgets"))
      assertSnapshot(request.query, "list-discussions")
    }
    "passes after and first into the discussions query" in {
      val request =
        GraphQl.listDiscussionsDocument(
          repo("acme", "widgets"),
          after = Present(cursor("cursor1")),
          first = pageSize(50)
        )
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
    "omits replies when reply depth is zero" in {
      val request = GraphQl.listDiscussionsDocument(repo("acme", "widgets"), replyDepth = ReplyDepth.none)
      assert(!request.query.contains("replies"))
    }
    "nests a second replies selection when reply depth is two" in {
      val request = GraphQl.listDiscussionsDocument(repo("acme", "widgets"), replyDepth = ReplyDepth(2))
      assertSnapshot(request.query, "list-discussions-depth-2")
    }
  }

  "GraphQl.listDiscussionRepliesDocument" - {
    "matches the blessed list-discussion-replies snapshot" in {
      val request = GraphQl.listDiscussionRepliesDocument(commentId("DC_1"), after = Present(cursor("cursor-1")))
      assertSnapshot(request.query, "list-discussion-replies")
    }
  }

  "GraphQl.listIssueCommentsDocument" - {
    "matches the blessed list-issue-comments snapshot" in {
      val request = GraphQl.listIssueCommentsDocument(repo("acme", "widgets"), issueNo(1))
      assertSnapshot(request.query, "list-issue-comments")
    }
    "passes after and first into the issue comments query" in {
      val request =
        GraphQl.listIssueCommentsDocument(
          repo("acme", "widgets"),
          issueNo(1),
          after = Present(cursor("cursor1")),
          first = pageSize(50)
        )
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listPullRequestCommentsDocument" - {
    "matches the blessed list-pull-request-comments snapshot" in {
      val request = GraphQl.listPullRequestCommentsDocument(repo("acme", "widgets"), prNo(3))
      assertSnapshot(request.query, "list-pull-request-comments")
    }
  }

  "GraphQl.listDiscussionCommentsDocument" - {
    "matches the blessed list-discussion-comments snapshot" in {
      val request = GraphQl.listDiscussionCommentsDocument(repo("acme", "widgets"), discNo(4))
      assertSnapshot(request.query, "list-discussion-comments")
    }
  }

  "GraphQl.getIssueDocument" - {
    "matches the blessed get-issue snapshot" in {
      val request = GraphQl.getIssueDocument(repo("acme", "widgets"), issueNo(1))
      assertSnapshot(request.query, "get-issue")
    }
  }

  "GraphQl.getPullRequestDocument" - {
    "matches the blessed get-pull-request snapshot" in {
      val request = GraphQl.getPullRequestDocument(repo("acme", "widgets"), prNo(3))
      assertSnapshot(request.query, "get-pull-request")
    }
  }

  "GraphQl.getDiscussionDocument" - {
    "matches the blessed get-discussion snapshot" in {
      val request = GraphQl.getDiscussionDocument(repo("acme", "widgets"), discNo(4))
      assertSnapshot(request.query, "get-discussion")
    }
  }

  "TokenProvider.const" - {
    "yields the given token" in {
      Token.parse("ghp_example") match
        case Present(token) =>
          run(TokenProvider.const(token).token).map {
            case Result.Success(got) =>
              assert(got == token)
              assert(got.toString == "Token(redacted)")
            case _ => assert(false)
          }
        case Absent => assert(false)
    }
  }

  "TokenProvider.flags" - {
    "names the flag morphir.connector.github.token" in {
      assert(token.name == "morphir.connector.github.token")
      assert(token.envName == "MORPHIR_CONNECTOR_GITHUB_TOKEN")
    }
    "fails Unauthorized when the flag value is blank" in
      run(TokenProvider.parseFlag("")).map {
        case Result.Failure(GitHubException.Unauthorized(detail)) =>
          assert(detail.nonEmpty)
        case _ => assert(false)
      }
    "parses a flag value into a Token" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      run(TokenProvider.parseFlag(secret)).map {
        case Result.Success(got) =>
          assert(got.toString == "Token(ghp_...abcd)")
        case _ => assert(false)
      }
    }
    "fails Unauthorized when the process flag is the default blank" in {
      if token.source == Flag.Source.Default then
        run(TokenProvider.flags.token).map {
          case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
          case _                                               => assert(false)
        }
      else assert(token.source != Flag.Source.Default)
    }
  }

  "TokenProvider.gitHubActions" - {
    "reads GITHUB_TOKEN" in
      assert(GITHUB_TOKEN.name == "GITHUB_TOKEN")
    "fails Unauthorized when GITHUB_TOKEN is blank" in
      run(TokenProvider.parseGitHubToken("")).map {
        case Result.Failure(GitHubException.Unauthorized(detail)) =>
          assert(detail.contains("GITHUB_TOKEN"))
        case _ => assert(false)
      }
    "parses a GITHUB_TOKEN value into a Token" in {
      val secret = "ghs_" + ("x" * 32) + "abcd"
      run(TokenProvider.parseGitHubToken(secret)).map {
        case Result.Success(got) =>
          assert(got.toString == "Token(ghs_...abcd)")
        case _ => assert(false)
      }
    }
    "uses the process GITHUB_TOKEN when it is set" in {
      if GITHUB_TOKEN().isEmpty then
        run(TokenProvider.gitHubActions.token).map {
          case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
          case _                                               => assert(false)
        }
      else
        run(TokenProvider.gitHubActions.token).map {
          case Result.Success(got) =>
            assert(got.toString.startsWith("Token("))
          case _ => assert(false)
        }
    }
  }

  "TokenProvider.gitHubCli" - {
    "asks gh auth token with no account flags by default" in
      assert(TokenProvider.gitHubCliArgs(Absent, Absent) == Chunk("auth", "token"))
    "maps a missing gh executable to Unauthorized" in
      run(
        PlatformGhAuth
          .forProgram("morphir-gh-command-that-does-not-exist")
          .stdout(Chunk("auth", "token"))
      ).map {
        case Result.Failure(GitHubException.Unauthorized(detail)) =>
          assert(detail.contains("not installed") || detail.contains("could not be started"))
        case _ => assert(false)
      }
    "passes --hostname and --user when present" in {
      val args = TokenProvider.gitHubCliArgs(Present("ada"), Present("github.com"))
      assert(args == Chunk("auth", "token", "--hostname", "github.com", "--user", "ada"))
    }
    "yields the token from gh stdout" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      val auth   = GhAuth.succeed(secret)
      run(TokenProvider.gitHubCli(Absent, Absent, auth).token).map {
        case Result.Success(got) => assert(got.toString == "Token(ghp_...abcd)")
        case _                   => assert(false)
      }
    }
    "fails Unauthorized when gh stdout is blank" in
      run(TokenProvider.gitHubCli(Absent, Absent, GhAuth.succeed("  ")).token).map {
        case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
        case _                                               => assert(false)
      }
    "fails Unauthorized when gh fails" in {
      val auth = GhAuth.fail(GitHubException.Unauthorized("gh: not logged in"))
      run(TokenProvider.gitHubCli(Absent, Absent, auth).token).map {
        case Result.Failure(GitHubException.Unauthorized(detail)) =>
          assert(detail.contains("not logged in"))
        case _ => assert(false)
      }
    }
    "records --user when the host names an account" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      val auth   = new GhAuth:
        def stdout(args: Chunk[String]) =
          if args == Chunk("auth", "token", "--user", "ada") then secret
          else Abort.fail(GitHubException.Unauthorized(s"unexpected args: $args"))
      run(TokenProvider.gitHubCli(Present("ada"), Absent, auth).token).map {
        case Result.Success(got) => assert(got.toString == "Token(ghp_...abcd)")
        case _                   => assert(false)
      }
    }
    "spawns gh rather than failing as an unlinked process floor" in
      run(TokenProvider.gitHubCli().token).map {
        case Result.Failure(GitHubException.Transport(_))    => assert(false)
        case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
        case Result.Success(_)                               => assert(true)
        case _                                               => assert(false)
      }
  }

  "TokenProvider.vault" - {
    "yields the token from SecretStore" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      val store  = SecretStore.const(("gh", "morphir", secret))
      runVault(store)(TokenProvider.vault("gh", "morphir").map(_.token)).map {
        case Result.Success(got) => assert(got.toString == "Token(ghp_...abcd)")
        case _                   => assert(false)
      }
    }
    "fails Unauthorized when the entry is missing" in {
      val store = SecretStore.const()
      runVault(store)(TokenProvider.vault("gh", "morphir").map(_.token)).map {
        case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
        case _                                               => assert(false)
      }
    }
    "fails Unauthorized when the stored value is blank" in {
      val store = SecretStore.const(("gh", "morphir", "  "))
      runVault(store)(TokenProvider.vault("gh", "morphir").map(_.token)).map {
        case Result.Failure(GitHubException.Unauthorized(_)) => assert(true)
        case _                                               => assert(false)
      }
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
    "is constructible from Env[TokenProvider] without calling GitHub" in {
      Token.parse("ghp_example") match
        case Present(token) =>
          run(Env.run(TokenProvider.const(token))(GithubClient.live)).map {
            case Result.Success(_) => assert(true)
            case _                 => assert(false)
          }
        case Absent => assert(false)
    }
  }

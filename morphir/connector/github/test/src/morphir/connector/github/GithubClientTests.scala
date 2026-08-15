package morphir.connector.github

import kyo.*
import kyo.test.*
import kyo.test.snapshot.*
import morphir.appkit.SecretStore
import morphir.connector.github.internal.GhAuth
import morphir.connector.github.internal.GraphQl

class GithubClientTests extends SnapshotTest[Any]:

  override def snapshotDir: String =
    SnapshotDir.value

  private def run[A](effect: A < (Abort[GithubError] & Async)): Result[GithubError, A] < Async =
    Abort.run[GithubError](effect)

  private def runVault[A](
      store: SecretStore
  )(effect: A < (Env[SecretStore] & Abort[GithubError] & Async)): Result[GithubError, A] < Async =
    Abort.run[GithubError](Env.run(store)(effect))

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

  "GithubClient.fixture" - {
    "returns the recorded issues" in {
      val issue  = Issue(1, "title", Present("body"), "https://example.test/1")
      val client = GithubClient.fixture(issues = Chunk(issue))
      run(client.listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) => assert(page.nodes == Chunk(issue))
        case _                    => assert(false)
      }
    }
    "returns empty pull requests and discussions by default" in {
      val client = GithubClient.fixture()
      val repo   = RepositoryRef("owner", "repo")
      run(client.listPullRequests(repo)).map {
        case Result.Success(page) => assert(page.nodes.isEmpty)
        case _                    => assert(false)
      }
    }
  }

  "GithubClient.recorded" - {
    "decodes issues from a GraphQL envelope" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      val client = GithubClient.recorded(issues = json)
      run(client.listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.nodes == Chunk(Issue(1, "title", Present("body"), "https://example.test/1")))
        case _ => assert(false)
      }
    }
    "decodes issue pageInfo from a GraphQL envelope" in {
      val json =
        """{"data":{"repository":{"issues":{"pageInfo":{"hasNextPage":true,"endCursor":"c2"},"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present("c2"))
          assert(page.nodes == Chunk(Issue(1, "title", Present("body"), "https://example.test/1")))
        case _ => assert(false)
      }
    }
    "treats missing issue pageInfo as the last page" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(!page.hasNextPage)
          assert(page.endCursor == Absent)
        case _ => assert(false)
      }
    }
    "treats a null issue body as Absent" in {
      val json =
        """{"data":{"repository":{"issues":{"nodes":[{"number":2,"title":"untitled","body":null,"url":"https://example.test/2"}]}}}}"""
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(page.nodes == Chunk(Issue(2, "untitled", Absent, "https://example.test/2")))
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
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Issue(
                number = 1,
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
      run(GithubClient.recorded(issues = json).listIssues(RepositoryRef("owner", "repo"))).map {
        case Result.Failure(GithubError.GraphQl(detail)) =>
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
      run(GithubClient.recorded(pullRequests = json).listPullRequests(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              PullRequest(
                number = 3,
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
      val repo   = RepositoryRef("owner", "repo")
      for
        prResult   <- run(client.listPullRequests(repo))
        discResult <- run(client.listDiscussions(repo))
      yield
        prResult match
          case Result.Success(page) =>
            assert(page.nodes == Chunk(PullRequest(3, "pr", Present("desc"), "https://example.test/3")))
          case _ => assert(false)
        discResult match
          case Result.Success(page) =>
            assert(page.nodes == Chunk(Discussion(4, "disc", Absent, "https://example.test/4")))
          case _ => assert(false)
    }
    "decodes who posted a discussion" in {
      val json =
        """{"data":{"repository":{"discussions":{"nodes":[{"number":4,"title":"disc","body":null,"url":"https://example.test/4","author":{"login":"ada","url":"https://github.com/ada"}}]}}}}"""
      run(GithubClient.recorded(discussions = json).listDiscussions(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Discussion(
                4,
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
      run(GithubClient.recorded(discussions = json).listDiscussions(RepositoryRef("owner", "repo"))).map {
        case Result.Success(page) =>
          assert(
            page.nodes == Chunk(
              Discussion(
                number = 4,
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
      run(GithubClient.recorded(issue = json).getIssue(RepositoryRef("owner", "repo"), 1)).map {
        case Result.Success(Present(issue)) =>
          assert(issue == Issue(1, "title", Present("body"), "https://example.test/1"))
        case _ => assert(false)
      }
    }
    "treats a missing issue as Absent" in {
      val json = """{"data":{"repository":{"issue":null}}}"""
      run(GithubClient.recorded(issue = json).getIssue(RepositoryRef("owner", "repo"), 99)).map {
        case Result.Success(Absent) => assert(true)
        case _                      => assert(false)
      }
    }
    "decodes a single pull request by number" in {
      val json =
        """{"data":{"repository":{"pullRequest":{"number":3,"title":"pr","body":"desc","url":"https://example.test/3"}}}}"""
      run(GithubClient.recorded(pullRequest = json).getPullRequest(RepositoryRef("owner", "repo"), 3)).map {
        case Result.Success(Present(pr)) =>
          assert(pr == PullRequest(3, "pr", Present("desc"), "https://example.test/3"))
        case _ => assert(false)
      }
    }
    "decodes a single discussion by number" in {
      val json =
        """{"data":{"repository":{"discussion":{"number":4,"title":"disc","body":null,"url":"https://example.test/4"}}}}"""
      run(GithubClient.recorded(discussion = json).getDiscussion(RepositoryRef("owner", "repo"), 4)).map {
        case Result.Success(Present(item)) =>
          assert(item == Discussion(4, "disc", Absent, "https://example.test/4"))
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
        GithubClient.recorded(discussionReplies = json).listDiscussionReplies("DC_1", after = Present("cursor-1"))
      ).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present("cursor-2"))
          assert(
            page.nodes == Chunk(
              DiscussionComment(
                id = Present("DC_2"),
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
          RepositoryRef("owner", "repo"),
          1,
          after = Present("c1")
        )
      ).map {
        case Result.Success(page) =>
          assert(page.hasNextPage)
          assert(page.endCursor == Present("c2"))
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
      val request = GraphQl.listIssuesDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-issues")
    }
    "passes after and first into the issues query" in {
      val request =
        GraphQl.listIssuesDocument(RepositoryRef("acme", "widgets"), after = Present("cursor1"), first = 50)
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listPullRequestsDocument" - {
    "matches the blessed list-pull-requests snapshot" in {
      val request = GraphQl.listPullRequestsDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-pull-requests")
    }
    "passes after and first into the pull requests query" in {
      val request =
        GraphQl.listPullRequestsDocument(RepositoryRef("acme", "widgets"), after = Present("cursor1"), first = 50)
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listDiscussionsDocument" - {
    "matches the blessed list-discussions snapshot" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"))
      assertSnapshot(request.query, "list-discussions")
    }
    "passes after and first into the discussions query" in {
      val request =
        GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"), after = Present("cursor1"), first = 50)
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
    "omits replies when reply depth is zero" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"), replyDepth = ReplyDepth.none)
      assert(!request.query.contains("replies"))
    }
    "nests a second replies selection when reply depth is two" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"), replyDepth = ReplyDepth(2))
      assertSnapshot(request.query, "list-discussions-depth-2")
    }
  }

  "GraphQl.listDiscussionRepliesDocument" - {
    "matches the blessed list-discussion-replies snapshot" in {
      val request = GraphQl.listDiscussionRepliesDocument("DC_1", after = Present("cursor-1"))
      assertSnapshot(request.query, "list-discussion-replies")
    }
  }

  "GraphQl.listIssueCommentsDocument" - {
    "matches the blessed list-issue-comments snapshot" in {
      val request = GraphQl.listIssueCommentsDocument(RepositoryRef("acme", "widgets"), 1)
      assertSnapshot(request.query, "list-issue-comments")
    }
    "passes after and first into the issue comments query" in {
      val request =
        GraphQl.listIssueCommentsDocument(RepositoryRef("acme", "widgets"), 1, after = Present("cursor1"), first = 50)
      assert(request.query.contains("after:\"cursor1\""))
      assert(request.query.contains("first:50"))
    }
  }

  "GraphQl.listPullRequestCommentsDocument" - {
    "matches the blessed list-pull-request-comments snapshot" in {
      val request = GraphQl.listPullRequestCommentsDocument(RepositoryRef("acme", "widgets"), 3)
      assertSnapshot(request.query, "list-pull-request-comments")
    }
  }

  "GraphQl.listDiscussionCommentsDocument" - {
    "matches the blessed list-discussion-comments snapshot" in {
      val request = GraphQl.listDiscussionCommentsDocument(RepositoryRef("acme", "widgets"), 4)
      assertSnapshot(request.query, "list-discussion-comments")
    }
  }

  "GraphQl.getIssueDocument" - {
    "matches the blessed get-issue snapshot" in {
      val request = GraphQl.getIssueDocument(RepositoryRef("acme", "widgets"), 1)
      assertSnapshot(request.query, "get-issue")
    }
  }

  "GraphQl.getPullRequestDocument" - {
    "matches the blessed get-pull-request snapshot" in {
      val request = GraphQl.getPullRequestDocument(RepositoryRef("acme", "widgets"), 3)
      assertSnapshot(request.query, "get-pull-request")
    }
  }

  "GraphQl.getDiscussionDocument" - {
    "matches the blessed get-discussion snapshot" in {
      val request = GraphQl.getDiscussionDocument(RepositoryRef("acme", "widgets"), 4)
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
        case Result.Failure(GithubError.Unauthorized(detail)) =>
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
          case Result.Failure(GithubError.Unauthorized(_)) => assert(true)
          case _                                           => assert(false)
        }
      else assert(token.source != Flag.Source.Default)
    }
  }

  "TokenProvider.gitHubActions" - {
    "reads GITHUB_TOKEN" in
      assert(GITHUB_TOKEN.name == "GITHUB_TOKEN")
    "fails Unauthorized when GITHUB_TOKEN is blank" in
      run(TokenProvider.parseGitHubToken("")).map {
        case Result.Failure(GithubError.Unauthorized(detail)) =>
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
          case Result.Failure(GithubError.Unauthorized(_)) => assert(true)
          case _                                           => assert(false)
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
        case Result.Failure(GithubError.Unauthorized(_)) => assert(true)
        case _                                           => assert(false)
      }
    "fails Unauthorized when gh fails" in {
      val auth = GhAuth.fail(GithubError.Unauthorized("gh: not logged in"))
      run(TokenProvider.gitHubCli(Absent, Absent, auth).token).map {
        case Result.Failure(GithubError.Unauthorized(detail)) =>
          assert(detail.contains("not logged in"))
        case _ => assert(false)
      }
    }
    "records --user when the host names an account" in {
      val secret = "ghp_" + ("x" * 32) + "abcd"
      val auth   = new GhAuth:
        def stdout(args: Chunk[String]) =
          if args == Chunk("auth", "token", "--user", "ada") then secret
          else Abort.fail(GithubError.Unauthorized(s"unexpected args: $args"))
      run(TokenProvider.gitHubCli(Present("ada"), Absent, auth).token).map {
        case Result.Success(got) => assert(got.toString == "Token(ghp_...abcd)")
        case _                   => assert(false)
      }
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
        case Result.Failure(GithubError.Unauthorized(_)) => assert(true)
        case _                                           => assert(false)
      }
    }
    "fails Unauthorized when the stored value is blank" in {
      val store = SecretStore.const(("gh", "morphir", "  "))
      runVault(store)(TokenProvider.vault("gh", "morphir").map(_.token)).map {
        case Result.Failure(GithubError.Unauthorized(_)) => assert(true)
        case _                                           => assert(false)
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

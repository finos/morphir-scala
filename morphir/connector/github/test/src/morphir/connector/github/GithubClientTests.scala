package morphir.connector.github

import kyo.*
import kyo.test.*
import kyo.test.snapshot.*
import morphir.connector.github.internal.GraphQl

class GithubClientTests extends SnapshotTest[Any]:

  override def snapshotDir: String =
    SnapshotDir.value

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
        case Result.Success(items) =>
          assert(
            items == Chunk(
              Issue(
                number = 1,
                title = "title",
                body = Present("body"),
                url = "https://example.test/1",
                author = ada,
                createdAt = Present(java.time.Instant.parse("2026-01-02T03:04:05Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-03T04:05:06Z")),
                labels = Chunk(Label("bug")),
                comments = Chunk(
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
        case Result.Success(items) =>
          assert(
            items == Chunk(
              PullRequest(
                number = 3,
                title = "pr",
                body = Present("desc"),
                url = "https://example.test/3",
                author = ada,
                createdAt = Present(java.time.Instant.parse("2026-01-02T03:04:05Z")),
                updatedAt = Present(java.time.Instant.parse("2026-01-03T04:05:06Z")),
                labels = Chunk(Label("enhancement")),
                comments = Chunk(
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
    "decodes who posted a discussion" in {
      val json =
        """{"data":{"repository":{"discussions":{"nodes":[{"number":4,"title":"disc","body":null,"url":"https://example.test/4","author":{"login":"ada","url":"https://github.com/ada"}}]}}}}"""
      run(GithubClient.recorded(discussions = json).listDiscussions(RepositoryRef("owner", "repo"))).map {
        case Result.Success(items) =>
          assert(
            items == Chunk(
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
        case Result.Success(items) =>
          assert(
            items == Chunk(
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
                comments = Chunk(comment)
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
    "omits replies when reply depth is zero" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"), ReplyDepth.none)
      assert(!request.query.contains("replies"))
    }
    "nests a second replies selection when reply depth is two" in {
      val request = GraphQl.listDiscussionsDocument(RepositoryRef("acme", "widgets"), ReplyDepth(2))
      assertSnapshot(request.query, "list-discussions-depth-2")
    }
  }

  "GraphQl.listDiscussionRepliesDocument" - {
    "matches the blessed list-discussion-replies snapshot" in {
      val request = GraphQl.listDiscussionRepliesDocument("DC_1", after = Present("cursor-1"))
      assertSnapshot(request.query, "list-discussion-replies")
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

  "GithubClient.live" - {
    "is constructible from a token without calling GitHub" in {
      Token.parse("ghp_example") match
        case Present(token) =>
          val _ = GithubClient.live(token)
          assert(true)
        case Absent => assert(false)
    }
  }

package morphir.connector.github

import kyo.*
import kyo.test.*

class GitHubTokenVerifierTests extends Test[Any]:

  private def run[A](effect: A < (Abort[GitHubException] & Async)): Result[GitHubException, A] < Async =
    Abort.run[GitHubException](effect)

  private val token: Token =
    Token.parse("synthetic-token-for-tests") match
      case Present(value) => value
      case Absent         => throw new AssertionError("test token must be valid")

  "GitHubLogin" - {
    "accepts GitHub login grammar and exposes its value" in {
      val expected = gitHubLogin"octo-cat"
      assert(GitHubLogin.parse("octo-cat") == Present(expected))
      assert(expected.value == "octo-cat")
    }

    "rejects leading, trailing, repeated, and overlong hyphens" in {
      assert(GitHubLogin.parse("-octocat").isEmpty)
      assert(GitHubLogin.parse("octocat-").isEmpty)
      assert(GitHubLogin.parse("octo--cat").isEmpty)
      assert(GitHubLogin.parse("a" * 40).isEmpty)
      assert(GitHubLogin.parse("octocåt").isEmpty)
    }
  }

  "GitHubTokenVerifier.recorded" - {
    "returns the authenticated viewer login" in {
      val verifier = GitHubTokenVerifier.recorded("""{"data":{"viewer":{"login":"octocat"}}}""")
      run(verifier.verify(token)).map {
        case Result.Success(login) => assert(login.value == "octocat")
        case _                     => assert(false)
      }
    }

    "redacts GraphQL error messages" in {
      val verifier =
        GitHubTokenVerifier.recorded("""{"data":null,"errors":[{"message":"synthetic-token-for-tests"}]}""")
      run(verifier.verify(token)).map {
        case Result.Failure(GitHubException.GraphQl(detail)) =>
          assert(detail == "GitHub token verification failed")
          assert(!detail.contains("synthetic-token-for-tests"))
        case _ => assert(false)
      }
    }

    "rejects an absent viewer" in {
      val verifier = GitHubTokenVerifier.recorded("""{"data":{"viewer":null}}""")
      run(verifier.verify(token)).map {
        case Result.Failure(GitHubException.GraphQl(_)) => assert(true)
        case _                                          => assert(false)
      }
    }

    "rejects a malformed viewer login" in {
      val verifier = GitHubTokenVerifier.recorded("""{"data":{"viewer":{"login":"octo--cat"}}}""")
      run(verifier.verify(token)).map {
        case Result.Failure(GitHubException.GraphQl(_)) => assert(true)
        case _                                          => assert(false)
      }
    }
  }

  "GitHubTokenVerifier HTTP failures" - {
    "maps 401 and 403 to a redacted authentication failure" in {
      val unauthorized = GitHubTokenVerifier.httpFailure(401)
      val forbidden    = GitHubTokenVerifier.httpFailure(403)
      assert(unauthorized == GitHubException.Unauthorized("GitHub token was rejected"))
      assert(forbidden == GitHubException.Unauthorized("GitHub token was rejected"))
      assert(!unauthorized.getMessage.contains("synthetic-token-for-tests"))
    }

    "maps transport failures without exposing response details" in {
      val failure = GitHubTokenVerifier.httpFailure(500)
      assert(failure == GitHubException.Transport("GitHub request failed"))
      assert(!failure.getMessage.contains("synthetic-token-for-tests"))
    }
  }

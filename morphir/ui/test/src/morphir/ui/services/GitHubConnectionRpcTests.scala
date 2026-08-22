package morphir.ui.services

import kyo.*
import kyo.test.*

class GitHubConnectionRpcTests extends Test[Any]:

  private val sentinel = tokenText(114, 112, 99, 45, 115, 101, 110, 116, 105, 110, 101, 108, 45, 116, 111, 107,
    101, 110, 45, 118, 97, 108, 117, 101)

  private def tokenText(codePoints: Int*): String =
    codePoints.iterator.map(_.toChar).mkString

  "TokenSubmission" - {
    "round-trips only through a connect request" in {
      val request = ConnectRequest(TokenSubmission.from(sentinel), remember = true)
      val encoded = Json.encode(request)

      assert(encoded.contains(sentinel))
      assert(Json.decode[ConnectRequest](encoded) == Result.succeed(request))
    }

    "redacts rendering and does not expose product fields" in {
      val submission     = TokenSubmission.from(sentinel)
      val inspected: Any = submission

      assert(submission.toString == "TokenSubmission(<redacted>)")
      assert(!inspected.isInstanceOf[Product])
      assert(!new RuntimeException(submission.toString).toString.contains(sentinel))
    }

    "compares by value while keeping a constant hash" in {
      val first = TokenSubmission.from(sentinel)
      val same  = TokenSubmission.from(sentinel)
      val other = TokenSubmission.from("another-token")

      assert(first == same)
      assert(first != other)
      assert(first.hashCode == 0)
      assert(other.hashCode == 0)
    }
  }

  "GitHub connection wire values" - {
    "round-trip every persistence and status case without rendering a submitted token" in {
      val persistences = Seq(
        ConnectionPersistence.Session,
        ConnectionPersistence.Device
      )
      val statuses = Seq(
        GitHubConnectionStatus.Disconnected,
        GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session),
        GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device),
        GitHubConnectionStatus.StoredCredentialRejected
      )
      val status             = GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
      val statusResponse     = StatusResponse(status)
      val connectResponse    = ConnectResponse(status)
      val disconnectResponse = DisconnectResponse()
      val values             = Seq(
        Json.encode(statusResponse),
        Json.encode(connectResponse),
        Json.encode(disconnectResponse),
        Json.encode(status),
        Json.encode(GitHubConnectionError.RejectedToken)
      )

      assert(Json.decode[StatusResponse](Json.encode(statusResponse)) == Result.succeed(statusResponse))
      assert(Json.decode[ConnectResponse](Json.encode(connectResponse)) == Result.succeed(connectResponse))
      assert(
        Json.decode[DisconnectResponse](Json.encode(disconnectResponse)) == Result.succeed(disconnectResponse)
      )
      assert(
        persistences.forall { expected =>
          Json.decode[ConnectionPersistence](Json.encode(expected)) == Result.succeed(expected)
        }
      )
      assert(
        statuses.forall { expected =>
          Json.decode[GitHubConnectionStatus](Json.encode(expected)) == Result.succeed(expected)
        }
      )
      assert(values.forall(encoded => !encoded.contains(sentinel)))
    }

    "round-trip every safe error case" in {
      val errors = Seq(
        GitHubConnectionError.RejectedToken,
        GitHubConnectionError.GitHubUnavailable,
        GitHubConnectionError.SecureStorageUnavailable,
        GitHubConnectionError.SecureStorageFailure,
        GitHubConnectionError.ExpiredLocalSession
      )

      assert(
        errors.forall { expected =>
          Json.decode[GitHubConnectionError](Json.encode(expected)) == Result.succeed(expected)
        }
      )
      assert(errors.forall(error => !Json.encode(error).contains(sentinel)))
    }

    "use the fixed safe error messages" in {
      val expected = Seq(
        GitHubConnectionError.RejectedToken            -> "GitHub rejected this token.",
        GitHubConnectionError.GitHubUnavailable        -> "GitHub is unavailable. Try again.",
        GitHubConnectionError.SecureStorageUnavailable -> "Secure storage is unavailable on this device.",
        GitHubConnectionError.SecureStorageFailure     -> "The credential could not be stored securely.",
        GitHubConnectionError.ExpiredLocalSession      -> "This local session expired. Reload the page."
      )

      assert(expected.forall((error, message) => error.getMessage == message))
      assert(expected.forall((error, _) => !error.toString.contains(sentinel)))
    }
  }

  "GitHubConnectionRpc" - {
    "uses the fixed method names" in {
      assert(GitHubConnectionRpc.methods.status == "morphir/github/status")
      assert(GitHubConnectionRpc.methods.connect == "morphir/github/connect")
      assert(GitHubConnectionRpc.methods.disconnect == "morphir/github/disconnect")
    }

    "round-trips all routes without returning the submission" in
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        val service = new GitHubConnectionService:
          def status()                                                = GitHubConnectionStatus.Disconnected
          def connect(submission: TokenSubmission, remember: Boolean) =
            GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
          def disconnect() = ()

        JsonRpcHandler.init(serverTransport, GitHubConnectionRpc.routes(service)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[StatusRequest, StatusResponse](GitHubConnectionRpc.methods.status, StatusRequest())
              .map { status =>
                client
                  .call[ConnectRequest, ConnectResponse](
                    GitHubConnectionRpc.methods.connect,
                    ConnectRequest(TokenSubmission.from(sentinel), remember = true)
                  )
                  .map { connected =>
                    client
                      .call[DisconnectRequest, DisconnectResponse](
                        GitHubConnectionRpc.methods.disconnect,
                        DisconnectRequest()
                      )
                      .map { disconnected =>
                        assert(status == StatusResponse(GitHubConnectionStatus.Disconnected))
                        assert(
                          connected == ConnectResponse(
                            GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
                          )
                        )
                        assert(disconnected == DisconnectResponse())
                        assert(!connected.toString.contains(sentinel))
                      }
                  }
              }
          }
        }
      }

    "returns a fixed safe failure envelope without the submitted token" in
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        val service = new GitHubConnectionService:
          def status()                                                = GitHubConnectionStatus.Disconnected
          def connect(submission: TokenSubmission, remember: Boolean) =
            Abort.fail(GitHubConnectionError.RejectedToken)
          def disconnect() = ()

        JsonRpcHandler.init(serverTransport, GitHubConnectionRpc.routes(service)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            Abort.run[JsonRpcError | Closed](
              client.call[ConnectRequest, ConnectResponse](
                GitHubConnectionRpc.methods.connect,
                ConnectRequest(TokenSubmission.from(sentinel), remember = false)
              )
            ).map {
              case Result.Failure(error: JsonRpcImplementationError) =>
                assert(error.code == -32002)
                assert(error.label == "Application error (-32002): GitHub connection error")
                assert(
                  error.message ==
                    "Server error (-32002): Application error (-32002): GitHub connection error"
                )
                assert(!error.toString.contains(sentinel))
                assert(!Json.encode[JsonRpcError](error).contains(sentinel))
                error.data match
                  case Present(data) =>
                    assert(Structure.decode[GitHubConnectionError](data) == Result.succeed(
                      GitHubConnectionError.RejectedToken
                    ))
                  case Absent => assert(false)
              case _ => assert(false)
            }
          }
        }
      }
  }
end GitHubConnectionRpcTests

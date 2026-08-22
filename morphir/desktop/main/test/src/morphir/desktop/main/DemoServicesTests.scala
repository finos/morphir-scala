package morphir.desktop.main

import kyo.*
import kyo.test.*
import morphir.appkit.*
import morphir.connector.github.*
import morphir.ui.github.GitHubConnectionCoordinator
import morphir.ui.services.*

class DemoServicesTests extends Test[Any]:

  val ws       = WorkspaceRef("/demo")
  val sentinel = "ghp_MORPHIR_TASK6_SENTINEL_TOKEN_1234567890"

  private final class MemoryVault extends SecretVault:
    private var stored: Maybe[Secret] = Absent

    def get(service: String, account: String) = stored

    def put(service: String, account: String, secret: Secret) = Sync.defer {
      stored = Present(secret)
    }

    def remove(service: String, account: String) = Sync.defer {
      stored = Absent
    }

  private final class CapturingTransport(delegate: JsonRpcTransport) extends JsonRpcTransport:
    var rawResponses = Chunk.empty[String]

    def send(envelope: JsonRpcEnvelope)(using Frame): Unit < (Async & Abort[Closed]) =
      delegate.send(envelope)

    def incoming(using Frame): Stream[JsonRpcEnvelope, Async & Abort[Closed]] =
      delegate.incoming.map { envelope =>
        rawResponses = rawResponses :+ Json.encode(envelope)
        envelope
      }

    def close(using Frame): Unit < Async =
      delegate.close

  "DemoServices" - {

    "ir.listPackages includes the Morphir SDK" in
      DemoServices.ir.listPackages(ws).map { packages =>
        assert(packages.exists(_.name == "Morphir.SDK"))
      }

    "knowledge.intentIndex includes this app's intent" in
      DemoServices.knowledge.intentIndex(ws).map { intents =>
        assert(intents.exists(_.number == "0030"))
      }

    "shell.appVersion reports the injected version" in
      DemoServices.shell("9.9.9").appVersion().map(v => assert(v == "9.9.9"))

    "routes cover all four services" in {
      val github = new GitHubConnectionService:
        def status()                                                = GitHubConnectionStatus.Disconnected
        def connect(submission: TokenSubmission, remember: Boolean) = GitHubConnectionStatus.Disconnected
        def disconnect()                                            = ()
      val names = DemoServices.routes("9.9.9", github).map(_.name)

      assert(
        names.contains(IrRpc.methods.listPackages) &&
          names.contains(KnowledgeRpc.methods.intentIndex) &&
          names.contains(ShellRpc.methods.appVersion) &&
          names.contains(GitHubConnectionRpc.methods.status)
      )
    }

    "routes carry safe GitHub connect, status and disconnect responses" in
      GitHubConnectionCoordinator
        .init(
          GitHubTokenVerifier.recorded("""{"data":{"viewer":{"login":"octocat"}}}"""),
          Present(MemoryVault())
        )
        .map { github =>
          JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
            val captured = CapturingTransport(clientTransport)
            JsonRpcHandler.init(serverTransport, DemoServices.routes("9.9.9", github)*).map { _ =>
              JsonRpcHandler.init(captured).map { client =>
                client
                  .call[ConnectRequest, ConnectResponse](
                    GitHubConnectionRpc.methods.connect,
                    ConnectRequest(TokenSubmission.from(sentinel), remember = true)
                  )
                  .map { connected =>
                    client
                      .call[StatusRequest, StatusResponse](GitHubConnectionRpc.methods.status, StatusRequest())
                      .map { status =>
                        client
                          .call[DisconnectRequest, DisconnectResponse](
                            GitHubConnectionRpc.methods.disconnect,
                            DisconnectRequest()
                          )
                          .map { disconnected =>
                            assert(
                              connected == ConnectResponse(
                                GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
                              )
                            )
                            assert(status == StatusResponse(connected.status))
                            assert(disconnected == DisconnectResponse())
                            assert(captured.rawResponses.size == 3)
                            assert(captured.rawResponses.forall(response => !response.contains(sentinel)))
                          }
                      }
                  }
              }
            }
          }
        }
  }
end DemoServicesTests

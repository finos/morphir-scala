package morphir.ui.services

import kyo.*
import kyo.test.*

class ShellServiceRpcTests extends Test[Any]:

  val fake = new ShellService:
    def pickWorkspace()    = Maybe(WorkspaceRef("/work/picked"))
    def recentWorkspaces() = Chunk(WorkspaceRef("/work/demo"))
    def appVersion()       = "0.0.1"

  "ShellRpc" - {

    "round-trips pickWorkspace, Maybe survives the wire" in
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        JsonRpcHandler.init(serverTransport, ShellRpc.routes(fake)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[PickWorkspaceRequest, PickWorkspaceResponse](ShellRpc.methods.pickWorkspace, PickWorkspaceRequest())
              .map(resp => assert(resp.workspace == Maybe(WorkspaceRef("/work/picked"))))
          }
        }
      }
  }
end ShellServiceRpcTests

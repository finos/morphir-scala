package morphir.ui.services

import kyo.*
import kyo.test.*

class IrServiceRpcTests extends Test[Any]:

  val ws = WorkspaceRef("/work/demo")

  val fake = new IrService:
    def listPackages(workspace: WorkspaceRef) =
      Chunk(PackageInfo("Morphir.SDK", 3))
    def listModules(workspace: WorkspaceRef, packageName: String) =
      Chunk(ModuleInfo(packageName, "List", 4, 21))
    def definition(workspace: WorkspaceRef, ref: DefinitionRef) =
      DefinitionDetail(ref, DefinitionKind.Value, "map : (a -> b) -> List a -> List b")

  "IrRpc" - {

    "round-trips listPackages over the in-memory transport" in
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        JsonRpcHandler.init(serverTransport, IrRpc.routes(fake)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[ListPackagesRequest, ListPackagesResponse](IrRpc.methods.listPackages, ListPackagesRequest(ws))
              .map(resp => assert(resp.packages == Chunk(PackageInfo("Morphir.SDK", 3))))
          }
        }
      }

    "round-trips definition over the in-memory transport" in {
      val ref = DefinitionRef("Morphir.SDK", "List", "map")
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        JsonRpcHandler.init(serverTransport, IrRpc.routes(fake)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[DefinitionRequest, DefinitionResponse](IrRpc.methods.definition, DefinitionRequest(ws, ref))
              .map(resp => assert(resp.definition.kind == DefinitionKind.Value))
          }
        }
      }
    }
  }
end IrServiceRpcTests

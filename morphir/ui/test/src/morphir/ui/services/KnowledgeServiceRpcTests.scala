package morphir.ui.services

import kyo.*
import kyo.test.*

class KnowledgeServiceRpcTests extends Test[Any]:

  val ws = WorkspaceRef("/work/demo")

  val fake = new KnowledgeService:
    def listBundles(workspace: WorkspaceRef) =
      Chunk(BundleInfo("intent", "Intent", 30))
    def concept(workspace: WorkspaceRef, ref: ConceptRef) =
      ConceptDetail(ref, "Intent", "Electron appkit", "body")
    def intentIndex(workspace: WorkspaceRef) =
      Chunk(IntentSummary("0025", "Electron appkit", "Refinement", "feature"))

  "KnowledgeRpc" - {

    "round-trips the intent index over the in-memory transport" in
      JsonRpcTransport.inMemory.map { (serverTransport, clientTransport) =>
        JsonRpcHandler.init(serverTransport, KnowledgeRpc.routes(fake)*).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[IntentIndexRequest, IntentIndexResponse](KnowledgeRpc.methods.intentIndex, IntentIndexRequest(ws))
              .map(resp => assert(resp.intents.headOption.map(_.state) == Some("Refinement")))
          }
        }
      }
  }
end KnowledgeServiceRpcTests

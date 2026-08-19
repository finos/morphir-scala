package morphir.ui.services

import kyo.*

final case class ListBundlesRequest(workspace: WorkspaceRef) derives CanEqual, Schema
final case class ListBundlesResponse(bundles: Chunk[BundleInfo]) derives CanEqual, Schema
final case class ConceptRequest(workspace: WorkspaceRef, ref: ConceptRef) derives CanEqual, Schema
final case class ConceptResponse(concept: ConceptDetail) derives CanEqual, Schema
final case class IntentIndexRequest(workspace: WorkspaceRef) derives CanEqual, Schema
final case class IntentIndexResponse(intents: Chunk[IntentSummary]) derives CanEqual, Schema

trait KnowledgeService:
  def listBundles(workspace: WorkspaceRef): Chunk[BundleInfo] < (Async & Abort[UiServiceError])
  def concept(workspace: WorkspaceRef, ref: ConceptRef): ConceptDetail < (Async & Abort[UiServiceError])
  def intentIndex(workspace: WorkspaceRef): Chunk[IntentSummary] < (Async & Abort[UiServiceError])

object KnowledgeRpc:
  object methods:
    val listBundles = "morphir/kb/listBundles"
    val concept     = "morphir/kb/concept"
    val intentIndex = "morphir/kb/intentIndex"

  def routes(service: KnowledgeService): Chunk[JsonRpcRoute[?, ?, ?]] =
    Chunk(
      JsonRpcRoute.request[ListBundlesRequest, ListBundlesResponse](methods.listBundles) { (req, _) =>
        service.listBundles(req.workspace).map(ListBundlesResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[ConceptRequest, ConceptResponse](methods.concept) { (req, _) =>
        service.concept(req.workspace, req.ref).map(ConceptResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[IntentIndexRequest, IntentIndexResponse](methods.intentIndex) { (req, _) =>
        service.intentIndex(req.workspace).map(IntentIndexResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage)
    )

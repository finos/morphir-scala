package morphir.ui.services

import kyo.*

final case class ListPackagesRequest(workspace: WorkspaceRef) derives CanEqual, Schema
final case class ListPackagesResponse(packages: Chunk[PackageInfo]) derives CanEqual, Schema
final case class ListModulesRequest(workspace: WorkspaceRef, packageName: String) derives CanEqual, Schema
final case class ListModulesResponse(modules: Chunk[ModuleInfo]) derives CanEqual, Schema
final case class DefinitionRequest(workspace: WorkspaceRef, ref: DefinitionRef) derives CanEqual, Schema
final case class DefinitionResponse(definition: DefinitionDetail) derives CanEqual, Schema

trait IrService:
  def listPackages(workspace: WorkspaceRef): Chunk[PackageInfo] < (Async & Abort[UiServiceError])
  def listModules(workspace: WorkspaceRef, packageName: String): Chunk[ModuleInfo] < (Async & Abort[UiServiceError])
  def definition(workspace: WorkspaceRef, ref: DefinitionRef): DefinitionDetail < (Async & Abort[UiServiceError])

object IrRpc:
  object methods:
    val listPackages = "morphir/ir/listPackages"
    val listModules  = "morphir/ir/listModules"
    val definition   = "morphir/ir/definition"

  def routes(service: IrService): Chunk[JsonRpcRoute[?, ?, ?]] =
    Chunk(
      JsonRpcRoute.request[ListPackagesRequest, ListPackagesResponse](methods.listPackages) { (req, _) =>
        service.listPackages(req.workspace).map(ListPackagesResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[ListModulesRequest, ListModulesResponse](methods.listModules) { (req, _) =>
        service.listModules(req.workspace, req.packageName).map(ListModulesResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[DefinitionRequest, DefinitionResponse](methods.definition) { (req, _) =>
        service.definition(req.workspace, req.ref).map(DefinitionResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage)
    )

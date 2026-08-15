package morphir.ui.services

import kyo.*

final case class PickWorkspaceRequest() derives CanEqual, Schema
final case class PickWorkspaceResponse(workspace: Maybe[WorkspaceRef]) derives CanEqual, Schema
final case class RecentWorkspacesRequest() derives CanEqual, Schema
final case class RecentWorkspacesResponse(workspaces: Chunk[WorkspaceRef]) derives CanEqual, Schema
final case class AppVersionRequest() derives CanEqual, Schema
final case class AppVersionResponse(version: String) derives CanEqual, Schema

trait ShellService:
  def pickWorkspace(): Maybe[WorkspaceRef] < (Async & Abort[UiServiceError])
  def recentWorkspaces(): Chunk[WorkspaceRef] < (Async & Abort[UiServiceError])
  def appVersion(): String < Async

object ShellRpc:
  object methods:
    val pickWorkspace    = "morphir/shell/pickWorkspace"
    val recentWorkspaces = "morphir/shell/recentWorkspaces"
    val appVersion       = "morphir/shell/appVersion"

  def routes(service: ShellService): Chunk[JsonRpcRoute[?, ?, ?]] =
    Chunk(
      JsonRpcRoute.request[PickWorkspaceRequest, PickWorkspaceResponse](methods.pickWorkspace) { (req, _) =>
        service.pickWorkspace().map(PickWorkspaceResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[RecentWorkspacesRequest, RecentWorkspacesResponse](methods.recentWorkspaces) { (req, _) =>
        service.recentWorkspaces().map(RecentWorkspacesResponse(_))
      }.error[UiServiceError](UiServiceError.wireCode, UiServiceError.wireMessage),
      JsonRpcRoute.request[AppVersionRequest, AppVersionResponse](methods.appVersion) { (req, _) =>
        service.appVersion().map(AppVersionResponse(_))
      }
    )

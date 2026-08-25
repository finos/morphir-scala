package morphir.ui.services

import kyo.*

final case class StatusRequest() derives CanEqual, Schema
final case class StatusResponse(status: GitHubConnectionStatus) derives CanEqual, Schema
final case class ConnectRequest(submission: TokenSubmission, remember: Boolean) derives CanEqual, Schema
final case class ConnectResponse(status: GitHubConnectionStatus) derives CanEqual, Schema
final case class DisconnectRequest() derives CanEqual, Schema
final case class DisconnectResponse() derives CanEqual, Schema

object GitHubConnectionRpc:
  object methods:
    val status     = "morphir/github/status"
    val connect    = "morphir/github/connect"
    val disconnect = "morphir/github/disconnect"

  val wireCode: Int       = -32002
  val wireMessage: String = "GitHub connection error"

  def routes(service: GitHubConnectionService): Chunk[JsonRpcRoute[?, ?, ?]] =
    Chunk(
      JsonRpcRoute.request[StatusRequest, StatusResponse](methods.status) { (_, _) =>
        service.status().map(StatusResponse(_))
      }.error[GitHubConnectionError](wireCode, wireMessage),
      JsonRpcRoute.request[ConnectRequest, ConnectResponse](methods.connect) { (request, _) =>
        service.connect(request.submission, request.remember).map(ConnectResponse(_))
      }.error[GitHubConnectionError](wireCode, wireMessage),
      JsonRpcRoute.request[DisconnectRequest, DisconnectResponse](methods.disconnect) { (_, _) =>
        service.disconnect().map(_ => DisconnectResponse())
      }.error[GitHubConnectionError](wireCode, wireMessage)
    )

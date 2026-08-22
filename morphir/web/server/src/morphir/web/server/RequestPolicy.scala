package morphir.web.server

import java.nio.charset.StandardCharsets
import kyo.*

object RequestPolicy:

  val maxRequestBytes = 64 * 1024

  val responseHeaders: HttpHeaders = HttpHeaders.init(
    Seq(
      "Cache-Control"   -> "no-store",
      "Referrer-Policy" -> "no-referrer"
    )
  )

  final case class Request(headers: HttpHeaders, body: String):
    override def toString: String = "Request(<redacted>)"

  enum Rejection derives CanEqual:
    case BadRequest, Unauthorized, PayloadTooLarge

  private final case class ExchangePayload(launch: String) derives Schema:
    override def toString: String = "ExchangePayload(<redacted>)"

  def authorizeRpc(
      request: Request,
      boundPort: Int,
      sessions: LaunchSessions
  )(using Frame): Result[Rejection, Unit] < Sync =
    authorizeApiRequest(request, boundPort) match
      case Result.Failure(error) => Result.fail(error)
      case Result.Panic(error)   => Result.panic(error)
      case Result.Success(_)     =>
        sessionCookie(request.headers) match
          case Absent              => Result.fail(Rejection.Unauthorized)
          case Present(rawSession) =>
            sessions.authenticate(rawSession).map { authenticated =>
              if authenticated then Result.succeed(())
              else Result.fail(Rejection.Unauthorized)
            }

  def authorizeExchange(
      request: Request,
      boundPort: Int,
      sessions: LaunchSessions
  )(using Frame): Result[Rejection, LaunchSessions.SessionCookie] < Sync =
    authorizeApiRequest(request, boundPort) match
      case Result.Failure(error) => Result.fail(error)
      case Result.Panic(error)   => Result.panic(error)
      case Result.Success(_)     =>
        Json.decode[ExchangePayload](request.body) match
          case Result.Success(payload) =>
            sessions.exchange(payload.launch).map {
              case Result.Success(cookie) => Result.succeed(cookie)
              case Result.Failure(_)      => Result.fail(Rejection.Unauthorized)
              case Result.Panic(error)    => Result.panic(error)
            }
          case Result.Failure(_)   => Result.fail(Rejection.BadRequest)
          case Result.Panic(error) => Result.panic(error)

  private[server] def authorizeApiRequest(request: Request, boundPort: Int): Result[Rejection, Unit] =
    if request.body.getBytes(StandardCharsets.UTF_8).length > maxRequestBytes then
      Result.fail(Rejection.PayloadTooLarge)
    else
      val expectedHost   = s"127.0.0.1:$boundPort"
      val expectedOrigin = s"http://127.0.0.1:$boundPort"
      if request.headers.getAll("Host") != Seq(expectedHost) then Result.fail(Rejection.Unauthorized)
      else if request.headers.getAll("Origin") != Seq(expectedOrigin) then Result.fail(Rejection.Unauthorized)
      else if !isJson(request.headers.getAll("Content-Type")) then Result.fail(Rejection.BadRequest)
      else Result.succeed(())

  private[server] def authenticateSession(
      headers: HttpHeaders,
      sessions: LaunchSessions
  )(using Frame): Boolean < Sync =
    sessionCookie(headers) match
      case Present(rawSession) => sessions.authenticate(rawSession)
      case Absent              => false

  private def isJson(contentTypes: Seq[String]): Boolean =
    contentTypes match
      case Seq(contentType) => contentType.takeWhile(_ != ';').trim.equalsIgnoreCase("application/json")
      case _                => false

  private def sessionCookie(headers: HttpHeaders): Maybe[String] =
    val values = headers.getAll("Cookie").flatMap { header =>
      header.split(';').iterator.map(_.trim).flatMap { part =>
        part.split("=", 2) match
          case Array("morphir_session", value) if value.nonEmpty => Iterator.single(value)
          case _                                                 => Iterator.empty
      }.toSeq
    }
    if values.size == 1 then Present(values.head) else Absent
end RequestPolicy

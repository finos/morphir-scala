package morphir.web.server

import kyo.*

final class HttpJsonRpcBridge private (
    inbound: Channel[HttpJsonRpcBridge.Inbound],
    hooks: HttpJsonRpcBridge.Hooks,
    createdAt: Frame
):

  import HttpJsonRpcBridge.*

  private val lock     = new Object
  private var closed   = false
  private var admitted = 0
  private var requests = Map.empty[JsonRpcId, RequestState]

  val transport: JsonRpcTransport = new JsonRpcTransport:
    def send(envelope: JsonRpcEnvelope)(using Frame): Unit < (Async & Abort[Closed]) =
      envelope match
        case response: JsonRpcResponse => completeResponse(response)
        case _                         => ()

    def incoming(using Frame): Stream[JsonRpcEnvelope, Async & Abort[Closed]] =
      inbound.stream().map { entry =>
        entry.notification match
          case Present(ticket) => releaseNotification(ticket).andThen(entry.envelope)
          case Absent          => entry.envelope
      }

    def close(using Frame): Unit < Async =
      HttpJsonRpcBridge.this.close

  def handle(body: String)(using Frame): Response < (Async & Abort[Closed]) =
    if body.getBytes(java.nio.charset.StandardCharsets.UTF_8).length > maxRequestBytes then
      Response(413, Absent, RequestPolicy.responseHeaders)
    else
      Json.decode[JsonRpcEnvelope](body) match
        case Result.Success(request: JsonRpcRequest)                         => handleRequest(request)
        case Result.Success(notification: JsonRpcNotification)               => handleNotification(notification)
        case Result.Success(_: JsonRpcResponse | _: JsonRpcMalformedMessage) =>
          Response(400, Absent, RequestPolicy.responseHeaders)
        case Result.Failure(_) => Response(400, Absent, RequestPolicy.responseHeaders)
        case Result.Panic(_)   => Response(400, Absent, RequestPolicy.responseHeaders)

  private def handleRequest(request: JsonRpcRequest)(using Frame): Response < (Async & Abort[Closed]) =
    Promise.init[JsonRpcResponse, Abort[Closed]].map { promise =>
      val registration = new RequestRegistration(promise)
      Sync.ensure(cleanupRequest(request.id, registration)) {
        Sync.defer(registerRequest(request.id, registration)).map {
          case Admission.Accepted  => enqueueRequest(request, registration)
          case Admission.Duplicate =>
            Response(400, Absent, RequestPolicy.responseHeaders)
          case Admission.Overloaded   => overloadResponse
          case Admission.BridgeClosed => failClosed
        }
      }
    }

  private def enqueueRequest(request: JsonRpcRequest, registration: RequestRegistration)(using
      Frame
  ): Response < (Async & Abort[Closed]) =
    hooks.beforeEnqueue(request).andThen {
      offerRequest(request, registration).map {
        case true =>
          registration.promise.get.map { response =>
            Response(200, Present(Json.encode(response)), RequestPolicy.responseHeaders)
          }
        case false => overloadResponse
      }
    }

  private def handleNotification(notification: JsonRpcNotification)(using
      Frame
  ): Response < (Async & Abort[Closed]) =
    val ticket = new NotificationAdmission
    Sync.ensure(releaseNotificationIfOwned(ticket)) {
      Sync.defer(registerNotification(ticket)).map {
        case Admission.Accepted =>
          hooks.beforeEnqueue(notification).andThen {
            offerNotification(notification, ticket).map {
              case true  => Response(204, Absent, RequestPolicy.responseHeaders)
              case false => overloadResponse
            }
          }
        case Admission.Overloaded   => overloadResponse
        case Admission.BridgeClosed => failClosed
        case Admission.Duplicate    => Response(400, Absent, RequestPolicy.responseHeaders)
      }
    }

  private def offerRequest(request: JsonRpcRequest, registration: RequestRegistration)(using
      Frame
  ): Boolean < (Sync & Abort[Closed]) =
    // One synchronous step makes interruption observe either rollback or a durable enqueued registration.
    Sync.Unsafe.defer {
      markEnqueued(request.id, registration)
      val result = inbound.unsafe.offer(Inbound(request, Absent))
      result.foldError(
        offered => if !offered then rollbackRequest(request.id, registration),
        _ => rollbackRequest(request.id, registration)
      )
      Abort.get(result)
    }

  private def offerNotification(notification: JsonRpcNotification, ticket: NotificationAdmission)(using
      Frame
  ): Boolean < (Sync & Abort[Closed]) =
    Sync.Unsafe.defer {
      val result = inbound.unsafe.offer(Inbound(notification, Present(ticket)))
      result.foldError(
        offered =>
          if offered then transferNotification(ticket)
          else releaseNotificationNow(ticket),
        _ => releaseNotificationNow(ticket)
      )
      Abort.get(result)
    }

  private def completeResponse(response: JsonRpcResponse)(using Frame): Unit < Async =
    Sync.defer {
      lock.synchronized {
        requests.get(response.id) match
          case Some(RequestState.Enqueued(registration)) =>
            requests = requests.removed(response.id)
            releaseAdmission()
            Some(registration.promise)
          case Some(RequestState.Abandoned) =>
            requests = requests.removed(response.id)
            releaseAdmission()
            None
          case Some(RequestState.Registered(_)) | None => None
      }
    }.map {
      case Some(promise) => promise.completeDiscard(Result.succeed(response))
      case None          => ()
    }

  private def registerRequest(id: JsonRpcId, registration: RequestRegistration): Admission =
    lock.synchronized {
      if closed then Admission.BridgeClosed
      else if requests.contains(id) then Admission.Duplicate
      else if admitted >= maxInFlight then Admission.Overloaded
      else
        admitted += 1
        requests = requests.updated(id, RequestState.Registered(registration))
        Admission.Accepted
    }

  private def markEnqueued(id: JsonRpcId, registration: RequestRegistration): Unit =
    lock.synchronized {
      requests.get(id) match
        case Some(RequestState.Registered(current)) if current eq registration =>
          requests = requests.updated(id, RequestState.Enqueued(registration))
        case _ => ()
    }

  private def rollbackRequest(id: JsonRpcId, registration: RequestRegistration): Unit =
    lock.synchronized {
      requests.get(id) match
        case Some(RequestState.Registered(current)) if current eq registration =>
          requests = requests.removed(id)
          releaseAdmission()
        case Some(RequestState.Enqueued(current)) if current eq registration =>
          requests = requests.removed(id)
          releaseAdmission()
        case _ => ()
    }

  private def cleanupRequest(id: JsonRpcId, registration: RequestRegistration)(using Frame): Unit < Sync =
    Sync.defer {
      lock.synchronized {
        requests.get(id) match
          case Some(RequestState.Registered(current)) if current eq registration =>
            requests = requests.removed(id)
            releaseAdmission()
          case Some(RequestState.Enqueued(current)) if current eq registration =>
            requests = requests.updated(id, RequestState.Abandoned)
          case _ => ()
      }
    }.andThen(hooks.afterRequestCleanup(id))

  private def registerNotification(ticket: NotificationAdmission): Admission =
    lock.synchronized {
      if closed then Admission.BridgeClosed
      else if admitted >= maxInFlight then Admission.Overloaded
      else
        admitted += 1
        ticket.state = NotificationState.Owned
        Admission.Accepted
    }

  private def transferNotification(ticket: NotificationAdmission): Unit =
    lock.synchronized {
      if ticket.state == NotificationState.Owned then ticket.state = NotificationState.Queued
    }

  private def releaseNotificationIfOwned(ticket: NotificationAdmission)(using Frame): Unit < Sync =
    Sync.defer {
      lock.synchronized {
        if ticket.state == NotificationState.Owned then
          ticket.state = NotificationState.Released
          releaseAdmission()
      }
    }

  private def releaseNotification(ticket: NotificationAdmission)(using Frame): Unit < Sync =
    Sync.defer(releaseNotificationNow(ticket))

  private def releaseNotificationNow(ticket: NotificationAdmission): Unit =
    lock.synchronized {
      if ticket.state != NotificationState.Released then
        ticket.state = NotificationState.Released
        releaseAdmission()
    }

  private def releaseAdmission(): Unit =
    if !closed then admitted -= 1

  def close(using Frame): Unit < Async =
    Sync.defer {
      lock.synchronized {
        if closed then Seq.empty
        else
          closed = true
          admitted = 0
          val promises = requests.values.collect {
            case RequestState.Registered(registration) => registration.promise
            case RequestState.Enqueued(registration)   => registration.promise
          }.toSeq
          requests = Map.empty
          promises
      }
    }.map { promises =>
      inbound.close.unit.andThen {
        Kyo.foreachDiscard(promises) { promise =>
          promise.completeDiscard(Result.fail(new Closed("HttpJsonRpcBridge", createdAt)))
        }
      }
    }

  private[server] def admissionState(using Frame): AdmissionState < Sync =
    Sync.defer {
      lock.synchronized {
        AdmissionState(
          admitted,
          requests.size,
          requests.values.count(_ == RequestState.Abandoned)
        )
      }
    }

  private def failClosed(using Frame): Nothing < Abort[Closed] =
    Abort.fail(new Closed("HttpJsonRpcBridge", createdAt))

object HttpJsonRpcBridge:

  val maxRequestBytes     = RequestPolicy.maxRequestBytes
  private val maxInFlight = 64

  final case class Response(status: Int, body: Maybe[String], headers: HttpHeaders) derives CanEqual

  private[server] final case class AdmissionState(admitted: Int, requests: Int, abandoned: Int) derives CanEqual

  private final case class Inbound(envelope: JsonRpcEnvelope, notification: Maybe[NotificationAdmission])

  private final class RequestRegistration(val promise: Promise[JsonRpcResponse, Abort[Closed]])

  private enum RequestState:
    case Registered(registration: RequestRegistration)
    case Enqueued(registration: RequestRegistration)
    case Abandoned

  private enum Admission:
    case Accepted
    case Duplicate
    case Overloaded
    case BridgeClosed

  private final class NotificationAdmission:
    var state: NotificationState = NotificationState.Unreserved

  private enum NotificationState:
    case Unreserved
    case Owned
    case Queued
    case Released

  private[server] final case class Hooks(
      beforeEnqueue: JsonRpcEnvelope => Unit < Async = _ => (),
      afterRequestCleanup: JsonRpcId => Unit < Sync = _ => ()
  )

  // 503 reports temporary bridge saturation. A 204 notification response means the bounded queue accepted it.
  private def overloadResponse: Response =
    Response(503, Absent, RequestPolicy.responseHeaders)

  def init(using frame: Frame): HttpJsonRpcBridge < Sync =
    init(maxInFlight)

  private[server] def init(capacity: Int)(using frame: Frame): HttpJsonRpcBridge < Sync =
    init(capacity, Hooks())

  private[server] def init(capacity: Int, hooks: Hooks)(using
      frame: Frame
  ): HttpJsonRpcBridge < Sync =
    Channel.initUnscoped[Inbound](capacity).map(new HttpJsonRpcBridge(_, hooks, frame))
end HttpJsonRpcBridge

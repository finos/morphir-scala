package morphir.web.renderer

import kyo.*
import org.scalajs.dom
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.util.{Failure, Success}

object FetchJsonRpcTransport:

  private val capacity = 64
  // Bounds concurrent browser requests independently of Kyo's single JSON-RPC writer fiber.
  private val workerCount = 8

  private[renderer] final case class Request(
      endpoint: String,
      body: String,
      contentType: String,
      credentials: String
  ):
    override def toString: String = "Request(<redacted>)"

  private[renderer] final case class Response(status: Int, body: String)

  private[renderer] enum TerminalCause derives CanEqual:
    case Unauthorized, Unavailable

  private[renderer] trait SessionTransport extends JsonRpcTransport:
    def terminalCause(using Frame): Maybe[TerminalCause] < Sync

  private[renderer] trait Fetch:
    def post(request: Request)(using Frame): Result[Throwable, Response] < Async

  def apply(endpoint: String = "/api/jsonrpc")(using Frame): SessionTransport < (Async & Scope) =
    init(LiveFetch, endpoint)

  private[renderer] def init(
      fetch: Fetch,
      endpoint: String = "/api/jsonrpc"
  )(using frame: Frame): SessionTransport < (Async & Scope) =
    Scope.acquireRelease(allocate(fetch, endpoint, frame))(_.close)

  private def allocate(fetch: Fetch, endpoint: String, createdAt: Frame)(using Frame): Transport < Sync =
    for
      outbound         <- Channel.initUnscoped[JsonRpcEnvelope](capacity)
      incoming         <- Channel.initUnscoped[JsonRpcEnvelope](capacity)
      terminalSignal   <- Channel.initUnscoped[Unit](1)
      terminalCause    <- AtomicRef.init[Maybe[TerminalCause]](Absent)
      closed           <- AtomicBoolean.init(false)
      workerFibers     <- AtomicRef.init(Seq.empty[Fiber[Unit, Any]])
      coordinatorFiber <- AtomicRef.init[Maybe[Fiber[Unit, Any]]](Absent)
      transport = new Transport(
        fetch,
        endpoint,
        createdAt,
        outbound,
        incoming,
        terminalSignal,
        terminalCause,
        closed,
        workerFibers,
        coordinatorFiber
      )
      workers     <- startWorkers(workerCount, transport, Seq.empty)
      _           <- workerFibers.set(workers)
      coordinator <- Fiber.initUnscoped(transport.coordinatorLoop)
      _           <- coordinatorFiber.set(Present(coordinator))
    yield transport

  private def startWorkers(
      remaining: Int,
      transport: Transport,
      workers: Seq[Fiber[Unit, Any]]
  )(using Frame): Seq[Fiber[Unit, Any]] < Sync =
    if remaining == 0 then workers
    else
      // The Scope-acquired Transport owns these fibers and interrupts them from close().
      Fiber.initUnscoped(transport.workerLoop).map { worker =>
        startWorkers(remaining - 1, transport, workers :+ worker)
      }

  private final class Transport(
      fetch: Fetch,
      endpoint: String,
      createdAt: Frame,
      outbound: Channel[JsonRpcEnvelope],
      incomingChannel: Channel[JsonRpcEnvelope],
      terminalSignal: Channel[Unit],
      terminalCauseRef: AtomicRef[Maybe[TerminalCause]],
      closed: AtomicBoolean,
      workerFibers: AtomicRef[Seq[Fiber[Unit, Any]]],
      coordinatorFiber: AtomicRef[Maybe[Fiber[Unit, Any]]]
  ) extends SessionTransport:

    def terminalCause(using Frame): Maybe[TerminalCause] < Sync = terminalCauseRef.get

    def send(envelope: JsonRpcEnvelope)(using Frame): Unit < (Async & Abort[Closed]) =
      closed.get.map {
        case true  => failClosed(createdAt)
        case false => outbound.put(envelope)
      }

    def incoming(using Frame): Stream[JsonRpcEnvelope, Async & Abort[Closed]] =
      incomingChannel.streamUntilClosed()

    def close(using Frame): Unit < Async =
      closeState(Absent).andThen {
        interruptWorkers.andThen {
          coordinatorFiber.get.map {
            case Present(fiber) => fiber.interrupt.unit
            case Absent         => ()
          }
        }.andThen(terminalSignal.close.unit)
      }

    private[FetchJsonRpcTransport] def workerLoop(using Frame): Unit < Async =
      outbound.streamUntilClosed(1).foreachChunk { chunk =>
        Kyo.foreachDiscard(chunk)(handle)
      }

    private[FetchJsonRpcTransport] def coordinatorLoop(using Frame): Unit < Async =
      Abort.run[Closed](terminalSignal.take).map {
        case Result.Success(_) => interruptWorkers
        case _                 => ()
      }

    private def handle(envelope: JsonRpcEnvelope)(using Frame): Unit < Async =
      closed.get.map {
        case true  => ()
        case false =>
          val request = Request(endpoint, Json.encode(envelope), "application/json", "same-origin")
          fetch.post(request).map {
            case Result.Success(Response(204, _)) if envelope.isInstanceOf[JsonRpcNotification] => ()
            case Result.Success(Response(200, body)) if envelope.isInstanceOf[JsonRpcRequest]   =>
              decodeResponse(envelope.asInstanceOf[JsonRpcRequest], body) match
                case Result.Success(response) => publish(response)
                case _                        => failTerminal(TerminalCause.Unavailable)
            case Result.Success(Response(401, _)) => failTerminal(TerminalCause.Unauthorized)
            case _                                => failTerminal(TerminalCause.Unavailable)
          }
      }

    private def publish(response: JsonRpcResponse)(using Frame): Unit < Async =
      closed.get.map {
        case true  => ()
        case false =>
          Abort.run[Closed](incomingChannel.put(response)).map {
            case Result.Panic(error) => Abort.panic(error)
            case _                   => ()
          }
      }

    private def failTerminal(cause: TerminalCause)(using Frame): Unit < Async =
      closeState(Present(cause)).map { first =>
        if first then
          Abort.run[Closed](terminalSignal.put(())).unit
        else ()
      }

    private def closeState(cause: Maybe[TerminalCause])(using Frame): Boolean < Sync =
      closed.compareAndSet(false, true).map { first =>
        if first then
          cause.fold(Kyo.unit)(value => terminalCauseRef.set(Present(value))).andThen {
            outbound.close.unit.andThen(incomingChannel.close.unit).andThen(true)
          }
        else false
      }

    private def interruptWorkers(using Frame): Unit < Sync =
      workerFibers.get.map(fibers => Kyo.foreachDiscard(fibers)(_.interrupt.unit))
  end Transport

  private def decodeResponse(request: JsonRpcRequest, body: String)(using Frame): Result[Throwable, JsonRpcResponse] =
    Json.decode[JsonRpcEnvelope](body) match
      case Result.Success(response: JsonRpcResponse) if response.id == request.id => Result.succeed(response)
      case Result.Success(_)   => Result.fail(new IllegalStateException("invalid response"))
      case Result.Failure(_)   => Result.fail(new IllegalStateException("invalid response"))
      case Result.Panic(error) => Result.panic(error)

  private def failClosed(createdAt: Frame)(using Frame): Nothing < Abort[Closed] =
    Abort.fail(new Closed("FetchJsonRpcTransport", createdAt))

  private[renderer] object LiveFetch extends Fetch:
    def post(request: Request)(using Frame): Result[Throwable, Response] < Async =
      val init = new dom.RequestInit {}
      init.method = dom.HttpMethod.POST
      init.headers = js.Dictionary("Content-Type" -> request.contentType)
      init.body = request.body
      init.credentials = dom.RequestCredentials.`same-origin`
      await(dom.Fetch.fetch(request.endpoint, init)).map {
        case Result.Success(response) =>
          await(response.text()).map {
            case Result.Success(body)  => Result.succeed(Response(response.status, body))
            case Result.Failure(error) => Result.fail(error)
            case Result.Panic(error)   => Result.panic(error)
          }
        case Result.Failure(error) => Result.fail(error)
        case Result.Panic(error)   => Result.panic(error)
      }

    private def await[A](promise: => js.Promise[A])(using Frame): Result[Throwable, A] < Async =
      try
        given ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
        val completed          = promise.toFuture.transform {
          case Success(value) => Success(Result.succeed(value))
          case Failure(error) => Success(Result.fail(error))
        }
        Async.fromFuture(completed)
      catch case error: Throwable => Result.fail(error)
end FetchJsonRpcTransport

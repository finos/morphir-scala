package morphir.web.renderer

import kyo.*
import kyo.test.*

class FetchJsonRpcTransportTests extends Test[Any]:

  final case class EchoRequest(value: String) derives CanEqual, Schema
  final case class EchoResponse(value: String) derives CanEqual, Schema

  private final class FakeFetch(respond: FetchJsonRpcTransport.Request => Result[
    Throwable,
    FetchJsonRpcTransport.Response
  ] < Async) extends FetchJsonRpcTransport.Fetch:
    var requests = List.empty[FetchJsonRpcTransport.Request]
    def post(request: FetchJsonRpcTransport.Request)(using
        Frame
    ): Result[Throwable, FetchJsonRpcTransport.Response] < Async = Async.defer {
      requests = requests :+ request
      respond(request)
    }

  private val rpcRequest = Json.decode[JsonRpcEnvelope](
    """{"jsonrpc":"2.0","id":7,"method":"echo","params":{"value":"morphir"}}"""
  ).getOrThrow

  private val rpcResponse =
    """{"jsonrpc":"2.0","id":7,"result":{"value":"rihprom"}}"""

  private val notification = Json.decode[JsonRpcEnvelope](
    """{"jsonrpc":"2.0","method":"echo","params":{"value":"notice"}}"""
  ).getOrThrow

  private def responseFor(request: FetchJsonRpcTransport.Request, value: String): FetchJsonRpcTransport.Response =
    val id = Json.decode[JsonRpcEnvelope](request.body).getOrThrow match
      case request: JsonRpcRequest => request.id
      case _                       => throw new IllegalArgumentException("expected request")
    val response = JsonRpcResponse(id, Present(Structure.encode(EchoResponse(value))), Absent, Absent)
    FetchJsonRpcTransport.Response(200, Json.encode[JsonRpcEnvelope](response))

  private def handlerCall(
      handler: JsonRpcHandler,
      value: String
  )(using Frame): Result[JsonRpcError | Closed, EchoResponse] < Async =
    Abort.run[JsonRpcError | Closed](handler.call[EchoRequest, EchoResponse]("echo", EchoRequest(value)))

  private def guarded[A](effect: => A < Async)(using Frame): Result[Timeout, A] < Async =
    Abort.run[Timeout](Async.timeout(2.seconds)(effect))

  private def guardedScoped[A](effect: => A < (Async & Scope))(using Frame): Result[Timeout, A] < Async =
    guarded(Scope.run(effect))

  private def isClosed(result: Result[JsonRpcError | Closed, EchoResponse]): Boolean =
    result match
      case Result.Failure(_: Closed) => true
      case _                         => false

  "FetchJsonRpcTransport" - {

    "posts one JSON envelope with same-origin credentials and emits a 200 response" in {
      val fetch = FakeFetch(_ => Result.succeed(FetchJsonRpcTransport.Response(200, rpcResponse)))
      FetchJsonRpcTransport.init(fetch).map { transport =>
        transport.send(rpcRequest).map { _ =>
          transport.incoming.take(1).run.map { received =>
            val sent = fetch.requests.head
            assert(fetch.requests.size == 1)
            assert(sent.endpoint == "/api/jsonrpc")
            assert(sent.contentType == "application/json")
            assert(sent.credentials == "same-origin")
            assert(sent.body == Json.encode(rpcRequest))
            assert(!sent.toString.contains(sent.body))
            assert(received.size == 1 && received.head.isInstanceOf[JsonRpcResponse])
          }
        }
      }
    }

    "accepts 204 for a notification" in
      Latch.init(1).map { posted =>
        val fetch = FakeFetch(_ => posted.release.andThen(Result.succeed(FetchJsonRpcTransport.Response(204, ""))))
        FetchJsonRpcTransport.init(fetch).map { transport =>
          transport.send(notification).map { _ =>
            posted.await.map(_ => assert(fetch.requests.size == 1))
          }
        }
      }

    "closes cleanly and rejects later sends" in {
      val fetch = FakeFetch(_ => Result.succeed(FetchJsonRpcTransport.Response(204, "")))
      FetchJsonRpcTransport.init(fetch).map { transport =>
        transport.close.map { _ =>
          Abort.run[Closed](transport.send(notification)).map { result =>
            assert(result.isFailure && fetch.requests.isEmpty)
          }
        }
      }
    }

    "terminates handler calls on network, HTTP, malformed, and wrong-id failures" in {
      val failures = Seq[FetchJsonRpcTransport.Request => Result[Throwable, FetchJsonRpcTransport.Response] < Async](
        _ => Result.fail(new RuntimeException("offline")),
        _ => Result.succeed(FetchJsonRpcTransport.Response(503, "unavailable")),
        _ => Result.succeed(FetchJsonRpcTransport.Response(204, "")),
        _ => Result.succeed(FetchJsonRpcTransport.Response(200, "not-json")),
        _ => Result.succeed(FetchJsonRpcTransport.Response(200, rpcResponse.replace("\"id\":7", "\"id\":99")))
      )

      Kyo.foreachDiscard(failures) { failure =>
        FetchJsonRpcTransport.init(FakeFetch(failure)).map { transport =>
          JsonRpcHandler.init(transport).map { handler =>
            guarded(handlerCall(handler, "failure")).map {
              case Result.Success(Result.Failure(error: Closed)) =>
                Abort.run[Closed](transport.send(notification)).map { laterNotification =>
                  assert(laterNotification.isFailure && !error.getMessage.contains("failure"))
                }
              case _ => assert(false)
            }
          }
        }
      }
    }

    "retains only a safe terminal category for unauthorized and unavailable failures" in {
      val cases = Seq(
        FakeFetch(_ => Result.succeed(FetchJsonRpcTransport.Response(401, "unauthorized-body-sentinel"))) ->
          FetchJsonRpcTransport.TerminalCause.Unauthorized,
        FakeFetch(_ => Result.succeed(FetchJsonRpcTransport.Response(503, "server-body-sentinel"))) ->
          FetchJsonRpcTransport.TerminalCause.Unavailable,
        FakeFetch(_ => Result.fail(new RuntimeException("network-detail-sentinel"))) ->
          FetchJsonRpcTransport.TerminalCause.Unavailable
      )

      Kyo.foreachDiscard(cases) { (fetch, expected) =>
        Scope.run {
          FetchJsonRpcTransport.init(fetch).map { transport =>
            JsonRpcHandler.init(transport).map { handler =>
              guarded(handlerCall(handler, "terminal-cause")).map { _ =>
                transport.terminalCause.map { cause =>
                  assert(cause == Present(expected))
                  assert(!cause.toString.contains("sentinel"))
                }
              }
            }
          }
        }
      }
    }

    "fails every pending handler call when one concurrent fetch fails" in
      Latch.init(1).map { firstEntered =>
        Latch.init(1).map { secondEntered =>
          Latch.init(1).map { neverComplete =>
            var invocation = 0
            val fetch      = FakeFetch { _ =>
              Async.defer {
                invocation += 1
                invocation
              }.map {
                case 1 => firstEntered.release.andThen(
                    secondEntered.await
                  ).andThen(Result.fail(new RuntimeException("offline")))
                case _ => secondEntered.release.andThen(
                    neverComplete.await
                  ).andThen(Result.fail(new RuntimeException("unreachable")))
              }
            }
            FetchJsonRpcTransport.init(fetch).map { transport =>
              JsonRpcHandler.init(transport).map { handler =>
                guardedScoped {
                  for
                    first  <- Fiber.init(handlerCall(handler, "first"))
                    _      <- firstEntered.await
                    second <- Fiber.init(handlerCall(handler, "second"))
                    _      <- secondEntered.await
                    one    <- first.get
                    two    <- second.get
                  yield assert(
                    isClosed(one) && isClosed(two)
                  )
                }.map {
                  case Result.Success(_) => assert(true)
                  case _                 => assert(false)
                }
              }
            }
          }
        }
      }

    "allows concurrent handler calls to complete out of order and correlate by id" in
      Latch.init(1).map { firstEntered =>
        Latch.init(1).map { secondEntered =>
          Latch.init(1).map { releaseFirst =>
            Latch.init(1).map { releaseSecond =>
              var invocation = 0
              val fetch      = FakeFetch { request =>
                Async.defer {
                  invocation += 1
                  invocation
                }.map {
                  case 1 => firstEntered.release.andThen(releaseFirst.await).andThen(Result.succeed(responseFor(
                      request,
                      "first-response"
                    )))
                  case _ => secondEntered.release.andThen(releaseSecond.await).andThen(Result.succeed(responseFor(
                      request,
                      "second-response"
                    )))
                }
              }
              FetchJsonRpcTransport.init(fetch).map { transport =>
                JsonRpcHandler.init(transport).map { handler =>
                  guardedScoped {
                    for
                      first  <- Fiber.init(handlerCall(handler, "first"))
                      _      <- firstEntered.await
                      second <- Fiber.init(handlerCall(handler, "second"))
                      _      <- secondEntered.await
                      _      <- releaseSecond.release
                      two    <- second.get
                      _      <- releaseFirst.release
                      one    <- first.get
                    yield assert(
                      one == Result.succeed(EchoResponse("first-response")) &&
                        two == Result.succeed(EchoResponse("second-response"))
                    )
                  }.map {
                    case Result.Success(_) => assert(true)
                    case _                 => assert(false)
                  }
                }
              }
            }
          }
        }
      }

    "closes during an in-flight fetch without publishing its later response" in
      Latch.init(1).map { entered =>
        Latch.init(1).map { release =>
          val fetch = FakeFetch { request =>
            entered.release.andThen(release.await).andThen(Result.succeed(responseFor(request, "too-late")))
          }
          FetchJsonRpcTransport.init(fetch).map { transport =>
            JsonRpcHandler.init(transport).map { handler =>
              guardedScoped {
                for
                  pending <- Fiber.init(handlerCall(handler, "close-race"))
                  _       <- entered.await
                  _       <- transport.close
                  _       <- transport.close
                  _       <- release.release
                  result  <- pending.get
                yield assert(isClosed(result))
              }.map {
                case Result.Success(_) => assert(true)
                case _                 => assert(false)
              }
            }
          }
        }
      }
  }
end FetchJsonRpcTransportTests

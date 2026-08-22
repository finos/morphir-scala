package morphir.web.server

import kyo.*
import kyo.test.*

class HttpJsonRpcBridgeTests extends Test[Any]:

  final case class EchoRequest(value: String) derives CanEqual, Schema
  final case class EchoResponse(value: String) derives CanEqual, Schema

  private val echoRoute = JsonRpcRoute.request[EchoRequest, EchoResponse]("echo") { (request, _) =>
    EchoResponse(request.value.reverse)
  }

  private def request(id: Long, value: String): String =
    s"""{"jsonrpc":"2.0","id":$id,"method":"echo","params":{"value":"$value"}}"""

  private def notification(value: String): String =
    s"""{"jsonrpc":"2.0","method":"echo","params":{"value":"$value"}}"""

  private def jsonRpcResponse(id: Long, value: String): JsonRpcResponse =
    JsonRpcResponse(
      JsonRpcId(id),
      Present(Structure.encode(EchoResponse(value))),
      Absent,
      Absent
    )

  private def responseId(response: HttpJsonRpcBridge.Response): Maybe[JsonRpcId] =
    response.body.flatMap { body =>
      Json.decode[JsonRpcEnvelope](body).toMaybe.flatMap {
        case value: JsonRpcResponse => Present(value.id)
        case _                      => Absent
      }
    }

  "HttpJsonRpcBridge" - {

    "correlates a request with the handler response" in
      HttpJsonRpcBridge.init.map { bridge =>
        JsonRpcHandler.init(bridge.transport, echoRoute).map { _ =>
          bridge.handle(request(7, "morphir")).map { response =>
            assert(response.status == 200)
            assert(responseId(response) == Present(JsonRpcId(7L)))
            assert(response.headers.get("Cache-Control") == Present("no-store"))
            assert(response.headers.get("Referrer-Policy") == Present("no-referrer"))
            assert(!response.headers.contains("Access-Control-Allow-Origin"))
          }
        }
      }

    "returns 204 after enqueueing a notification" in
      Latch.init(1).map { observed =>
        val notification = JsonRpcRoute.notification[EchoRequest]("echo") { (_, _) => observed.release }
        HttpJsonRpcBridge.init.map { bridge =>
          JsonRpcHandler.init(bridge.transport, notification).map { _ =>
            bridge
              .handle("""{"jsonrpc":"2.0","method":"echo","params":{"value":"notice"}}""")
              .map { response =>
                observed.await.map { _ =>
                  assert(response.status == 204 && response.body.isEmpty)
                }
              }
          }
        }
      }

    "correlates concurrent replies that complete out of order" in
      Latch.init(1).map { firstEntered =>
        Latch.init(1).map { releaseFirst =>
          val controlled = JsonRpcRoute.request[EchoRequest, EchoResponse]("echo") { (input, _) =>
            if input.value == "first" then
              firstEntered.release.andThen(releaseFirst.await).andThen(EchoResponse("first-response"))
            else EchoResponse("second-response")
          }
          HttpJsonRpcBridge.init.map { bridge =>
            JsonRpcHandler.init(bridge.transport, controlled).map { _ =>
              for
                firstFiber  <- Fiber.initUnscoped(bridge.handle(request(1, "first")))
                _           <- firstEntered.await
                secondFiber <- Fiber.initUnscoped(bridge.handle(request(2, "second")))
                second      <- secondFiber.get
                _           <- releaseFirst.release
                first       <- firstFiber.get
              yield assert(
                responseId(first) == Present(JsonRpcId(1L)) &&
                  responseId(second) == Present(JsonRpcId(2L))
              )
            }
          }
        }
      }

    "rejects a 65th in-flight request immediately" in
      Latch.init(64).map { entered =>
        Latch.init(1).map { neverRelease =>
          val blocked = JsonRpcRoute.request[EchoRequest, EchoResponse]("echo") { (input, _) =>
            entered.release.andThen(neverRelease.await).andThen(EchoResponse(input.value))
          }
          HttpJsonRpcBridge.init.map { bridge =>
            JsonRpcHandler.init(bridge.transport, blocked).map { _ =>
              Kyo.foreach(1 to 64) { id =>
                Fiber.initUnscoped(bridge.handle(request(id.toLong, s"request-$id")))
              }.map { _ =>
                entered.await.map { _ =>
                  bridge.admissionState.map { beforeOverload =>
                    Fiber.initUnscoped(bridge.handle(request(65, "overload"))).map { excess =>
                      excess.block(250.millis).map { result =>
                        bridge.admissionState.map { afterOverload =>
                          bridge.close.map { _ =>
                            assert(
                              beforeOverload.admitted == 64 &&
                                beforeOverload.requests == 64 &&
                                afterOverload == beforeOverload &&
                                (result match
                                  case Result.Success(response) =>
                                    response.status == 503 &&
                                    response.body.isEmpty &&
                                    response.headers.get("Cache-Control") == Present("no-store") &&
                                    response.headers.get("Referrer-Policy") == Present("no-referrer")
                                  case _ => false)
                            )
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

    "rolls back request admission when the HTTP fiber is cancelled before enqueue" in
      Latch.init(1).map { registered =>
        Latch.init(1).map { neverEnqueue =>
          Latch.init(1).map { cleaned =>
            val hooks = HttpJsonRpcBridge.Hooks(
              beforeEnqueue = _ => registered.release.andThen(neverEnqueue.await),
              afterRequestCleanup = _ => cleaned.release
            )
            HttpJsonRpcBridge.init(64, hooks).map { bridge =>
              Fiber.initUnscoped(bridge.handle(request(70, "cancel-before-enqueue"))).map { pending =>
                registered.await.map { _ =>
                  bridge.admissionState.map { beforeCancel =>
                    pending.interrupt.map { _ =>
                      pending.getResult.map { _ =>
                        cleaned.await.map { _ =>
                          bridge.admissionState.map { afterCancel =>
                            bridge.close.map { _ =>
                              assert(
                                beforeCancel.admitted == 1 &&
                                  beforeCancel.requests == 1 &&
                                  afterCancel.admitted == 0 &&
                                  afterCancel.requests == 0 &&
                                  afterCancel.abandoned == 0
                              )
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

    "keeps a cancelled enqueued request tombstoned until its late response is drained" in
      Latch.init(1).map { cleaned =>
        val hooks = HttpJsonRpcBridge.Hooks(afterRequestCleanup = _ => cleaned.release)
        HttpJsonRpcBridge.init(64, hooks).map { bridge =>
          for
            firstInbound  <- Fiber.initUnscoped(bridge.transport.incoming.take(1).run)
            first         <- Fiber.initUnscoped(bridge.handle(request(80, "first")))
            _             <- firstInbound.get
            _             <- first.interrupt
            _             <- first.getResult
            _             <- cleaned.await
            abandoned     <- bridge.admissionState
            duplicate     <- bridge.handle(request(80, "must-not-reuse-yet"))
            _             <- bridge.transport.send(jsonRpcResponse(80, "late-old-response"))
            drained       <- bridge.admissionState
            reusedInbound <- Fiber.initUnscoped(bridge.transport.incoming.take(1).run)
            reusedFiber   <- Fiber.initUnscoped(bridge.handle(request(80, "safe-reuse")))
            _             <- reusedInbound.get
            _             <- bridge.transport.send(jsonRpcResponse(80, "new-response"))
            reused        <- reusedFiber.get
            _             <- bridge.close
          yield assert(
            abandoned.admitted == 1 &&
              abandoned.abandoned == 1 &&
              duplicate.status == 400 &&
              drained.admitted == 0 &&
              drained.requests == 0 &&
              reused.status == 200 &&
              responseId(reused) == Present(JsonRpcId(80L))
          )
        }
      }

    "bounds queued notifications and rejects overflow immediately" in
      HttpJsonRpcBridge.init.map { bridge =>
        Kyo.foreach(1 to 64) { id =>
          bridge.handle(notification(s"notice-$id"))
        }.map { accepted =>
          bridge.admissionState.map { full =>
            Fiber.initUnscoped(bridge.handle(notification("overload"))).map { excess =>
              excess.block(250.millis).map { result =>
                bridge.close.map { _ =>
                  assert(
                    accepted.forall(_.status == 204) &&
                      full.admitted == 64 &&
                      full.requests == 0 &&
                      (result match
                        case Result.Success(response) => response.status == 503
                        case _                        => false)
                  )
                }
              }
            }
          }
        }
      }

    "close clears admitted requests, tombstones, and queued notifications" in
      Latch.init(1).map { cleaned =>
        val hooks = HttpJsonRpcBridge.Hooks(afterRequestCleanup = _ => cleaned.release)
        HttpJsonRpcBridge.init(64, hooks).map { bridge =>
          for
            inbound      <- Fiber.initUnscoped(bridge.transport.incoming.take(1).run)
            requestFiber <- Fiber.initUnscoped(bridge.handle(request(90, "abandon")))
            _            <- inbound.get
            _            <- requestFiber.interrupt
            _            <- requestFiber.getResult
            _            <- cleaned.await
            notification <- bridge.handle(notification("queued"))
            beforeClose  <- bridge.admissionState
            _            <- bridge.close
            afterClose   <- bridge.admissionState
          yield assert(
            notification.status == 204 &&
              beforeClose.admitted == 2 &&
              beforeClose.requests == 1 &&
              beforeClose.abandoned == 1 &&
              afterClose.admitted == 0 &&
              afterClose.requests == 0 &&
              afterClose.abandoned == 0
          )
        }
      }

    "rejects malformed JSON and inbound response envelopes" in
      HttpJsonRpcBridge.init.map { bridge =>
        bridge.handle("not-json").map { malformed =>
          bridge.handle("""{"jsonrpc":"2.0","id":1,"result":{}}""").map { response =>
            assert(malformed.status == 400 && response.status == 400)
          }
        }
      }

    "accepts the exact 64 KiB UTF-8 boundary and rejects one byte over and multibyte overflow" in
      HttpJsonRpcBridge.init.map { bridge =>
        val emptyBytes = notification("").getBytes(java.nio.charset.StandardCharsets.UTF_8).length
        val exact      = notification("x" * (HttpJsonRpcBridge.maxRequestBytes - emptyBytes))
        val oneOver    = notification("x" * (HttpJsonRpcBridge.maxRequestBytes - emptyBytes + 1))
        val multibyte  = notification("é" * (HttpJsonRpcBridge.maxRequestBytes - emptyBytes))
        bridge.handle(exact).map { accepted =>
          bridge.handle(oneOver).map { rejected =>
            bridge.handle(multibyte).map { rejectedMultibyte =>
              assert(exact.getBytes(java.nio.charset.StandardCharsets.UTF_8).length ==
                HttpJsonRpcBridge.maxRequestBytes)
              assert(oneOver.getBytes(java.nio.charset.StandardCharsets.UTF_8).length ==
                HttpJsonRpcBridge.maxRequestBytes + 1)
              assert(accepted.status == 204)
              assert(rejected.status == 413 && rejectedMultibyte.status == 413)
            }
          }
        }
      }

    "rejects a duplicate request id while preserving and then closing the original" in
      Latch.init(1).map { entered =>
        Latch.init(1).map { neverRelease =>
          val blocked = JsonRpcRoute.request[EchoRequest, EchoResponse]("echo") { (input, _) =>
            entered.release.andThen(neverRelease.await).andThen(EchoResponse(input.value))
          }
          HttpJsonRpcBridge.init.map { bridge =>
            JsonRpcHandler.init(bridge.transport, blocked).map { _ =>
              for
                original  <- Fiber.init(bridge.handle(request(10, "original")))
                _         <- entered.await
                duplicate <- bridge.handle(request(10, "duplicate"))
                _         <- bridge.close
                result    <- original.getResult
              yield assert(
                duplicate.status == 400 &&
                  (result match
                    case Result.Failure(_: Closed) => true
                    case _                         => false)
              )
            }
          }
        }
      }

    "close fails a request registered before enqueue" in
      Latch.init(1).map { registered =>
        Latch.init(1).map { releaseEnqueue =>
          val hooks = HttpJsonRpcBridge.Hooks(
            beforeEnqueue = _ => registered.release.andThen(releaseEnqueue.await)
          )
          HttpJsonRpcBridge.init(64, hooks).map { bridge =>
            Fiber.init(bridge.handle(request(11, "blocked-before-enqueue"))).map { pending =>
              registered.await.map { _ =>
                bridge.close.map { _ =>
                  releaseEnqueue.release.map { _ =>
                    pending.getResult.map { result =>
                      assert(result match
                        case Result.Failure(_: Closed) => true
                        case _                         => false)
                    }
                  }
                }
              }
            }
          }
        }
      }

    "closing fails every pending request with Closed" in
      Latch.init(1).map { entered =>
        Latch.init(1).map { neverRelease =>
          val blocked = JsonRpcRoute.request[EchoRequest, EchoResponse]("echo") { (input, _) =>
            entered.release.andThen(neverRelease.await).andThen(EchoResponse(input.value))
          }
          HttpJsonRpcBridge.init.map { bridge =>
            JsonRpcHandler.init(bridge.transport, blocked).map { _ =>
              for
                pending <- Fiber.initUnscoped(bridge.handle(request(9, "blocked")))
                _       <- entered.await
                _       <- bridge.close
                result  <- pending.getResult
              yield assert(result match
                case Result.Failure(_: Closed) => true
                case _                         => false)
            }
          }
        }
      }
  }
end HttpJsonRpcBridgeTests

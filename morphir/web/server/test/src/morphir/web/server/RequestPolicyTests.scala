package morphir.web.server

import kyo.*
import kyo.test.*

class RequestPolicyTests extends Test[Any]:

  private val port = 45871

  private final class DeterministicEntropy:
    private var seed         = 0
    val random: SecureRandom =
      import AllowUnsafe.embrace.danger
      SecureRandom(new SecureRandom.Unsafe:
        def nextBytes(length: Int)(using AllowUnsafe): Span[Byte] =
          seed += 1
          Span.from(Array.tabulate(length)(i => (seed + i).toByte)))

  private def request(
      body: String,
      overrides: Seq[(String, String)] = Seq.empty,
      omitted: Set[String] = Set.empty
  ): RequestPolicy.Request =
    val defaults = Seq(
      "Host"         -> s"127.0.0.1:$port",
      "Origin"       -> s"http://127.0.0.1:$port",
      "Content-Type" -> "application/json"
    )
    val overriddenNames = overrides.map(_._1.toLowerCase).toSet
    RequestPolicy.Request(
      HttpHeaders.init(
        defaults.filterNot((name, _) => overriddenNames.contains(name.toLowerCase) || omitted.contains(name)) ++
          overrides
      ),
      body
    )

  private def launchBody(value: String): String = s"""{"launch":"$value"}"""

  "RequestPolicy" - {

    "authorizes exact loopback RPC requests and JSON media types with parameters" in {
      val entropy = DeterministicEntropy()
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              RequestPolicy.authorizeExchange(request(launchBody(launch.value)), port, sessions).map {
                case Result.Success(cookie) =>
                  val raw = cookie.headerValue.stripPrefix("morphir_session=").takeWhile(_ != ';')
                  val rpc = request(
                    "{}",
                    Seq(
                      "Content-Type" -> "Application/JSON; charset=utf-8",
                      "Cookie"       -> s"other=ok; morphir_session=$raw"
                    )
                  )
                  RequestPolicy.authorizeRpc(rpc, port, sessions).map(result => assert(result.isSuccess))
                case _ => assert(false)
              }
            }
          }
        }
      }
    }

    "rejects wrong or missing Host, Origin, cookie, and content type" in {
      val entropy = DeterministicEntropy()
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              RequestPolicy.authorizeExchange(request(launchBody(launch.value)), port, sessions).map {
                case Result.Success(cookie) =>
                  val raw        = cookie.headerValue.stripPrefix("morphir_session=").takeWhile(_ != ';')
                  val candidates = Seq(
                    request("{}", Seq("Host" -> s"localhost:$port", "Cookie" -> s"morphir_session=$raw")),
                    request("{}", Seq("Cookie" -> s"morphir_session=$raw"), Set("Host")),
                    request("{}", Seq("Origin" -> "http://example.test", "Cookie" -> s"morphir_session=$raw")),
                    request("{}", Seq("Cookie" -> s"morphir_session=$raw"), Set("Origin")),
                    request("{}"),
                    request("{}", Seq("Cookie" -> "morphir_session=wrong")),
                    request("{}", Seq("Cookie" -> s"morphir_session=$raw", "Content-Type" -> "text/plain")),
                    request("{}", Seq("Cookie" -> s"morphir_session=$raw"), Set("Content-Type"))
                  )
                  Kyo.foreach(candidates)(candidate => RequestPolicy.authorizeRpc(candidate, port, sessions)).map {
                    results =>
                      assert(results.forall(_.isFailure))
                  }
                case _ => assert(false)
              }
            }
          }
        }
      }
    }

    "rejects duplicate Host, Origin, Content-Type, and session cookies" in {
      val entropy = DeterministicEntropy()
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              RequestPolicy.authorizeExchange(request(launchBody(launch.value)), port, sessions).map {
                case Result.Success(cookie) =>
                  val raw        = cookie.headerValue.stripPrefix("morphir_session=").takeWhile(_ != ';')
                  val candidates = Seq(
                    request(
                      "{}",
                      Seq(
                        "Host"   -> s"127.0.0.1:$port",
                        "Host"   -> s"127.0.0.1:$port",
                        "Cookie" -> s"morphir_session=$raw"
                      )
                    ),
                    request(
                      "{}",
                      Seq(
                        "Origin" -> s"http://127.0.0.1:$port",
                        "Origin" -> s"http://127.0.0.1:$port",
                        "Cookie" -> s"morphir_session=$raw"
                      )
                    ),
                    request(
                      "{}",
                      Seq(
                        "Content-Type" -> "application/json",
                        "Content-Type" -> "application/json; charset=utf-8",
                        "Cookie"       -> s"morphir_session=$raw"
                      )
                    ),
                    request(
                      "{}",
                      Seq(
                        "Cookie" -> s"morphir_session=$raw",
                        "Cookie" -> s"morphir_session=$raw"
                      )
                    )
                  )
                  Kyo.foreach(candidates)(candidate => RequestPolicy.authorizeRpc(candidate, port, sessions)).map {
                    results =>
                      assert(results.forall(_.isFailure))
                  }
                case _ => assert(false)
              }
            }
          }
        }
      }
    }

    "checks Host, Origin and media type before consuming the one-use launch" in {
      val entropy = DeterministicEntropy()
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              val rejected = request(
                launchBody(launch.value),
                Seq("Origin" -> "http://attacker.test")
              )
              RequestPolicy.authorizeExchange(rejected, port, sessions).map { first =>
                RequestPolicy.authorizeExchange(request(launchBody(launch.value)), port, sessions).map { second =>
                  assert(first.isFailure && second.isSuccess)
                }
              }
            }
          }
        }
      }
    }

    "rejects malformed and oversized exchange bodies without consuming the launch" in {
      val entropy = DeterministicEntropy()
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              RequestPolicy.authorizeExchange(request("not-json"), port, sessions).map { malformed =>
                val oversized = request("x" * (RequestPolicy.maxRequestBytes + 1))
                RequestPolicy.authorizeExchange(oversized, port, sessions).map { tooLarge =>
                  RequestPolicy.authorizeExchange(request(launchBody(launch.value)), port, sessions).map { accepted =>
                    assert(malformed.isFailure && tooLarge.isFailure && accepted.isSuccess)
                  }
                }
              }
            }
          }
        }
      }
    }

    "provides fixed no-store and referrer headers without CORS" in {
      val headers = RequestPolicy.responseHeaders
      assert(headers.get("Cache-Control") == Present("no-store"))
      assert(headers.get("Referrer-Policy") == Present("no-referrer"))
      assert(!headers.contains("Access-Control-Allow-Origin"))
    }

    "redacts request bodies from rendering" in {
      val sentinel = "launch-secret-sentinel"
      assert(!request(launchBody(sentinel)).toString.contains(sentinel))
    }
  }
end RequestPolicyTests

package morphir.web.server

import java.io.{BufferedInputStream, ByteArrayOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import kyo.*
import kyo.test.*
import morphir.connector.github.GitHubTokenVerifier
import morphir.ui.services.{ConnectRequest, TokenSubmission}

class WebHostTests extends Test[Any]:

  private val csp =
    "default-src 'self'; script-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"

  private final class FakeLauncher(opened: Boolean = true, panicOnOpen: Boolean = false)
      extends WebHost.BrowserLauncher:
    private val lock       = new Object
    private var openedUrls = Vector.empty[String]

    def open(url: String)(using Frame): Boolean < Async = Async.defer {
      lock.synchronized {
        openedUrls = openedUrls :+ url
      }
      if panicOnOpen then throw new IllegalStateException(s"launcher failed for $url")
      opened
    }

    def urls: Vector[String] = lock.synchronized {
      openedUrls
    }

    override def toString: String = "FakeLauncher(<redacted>)"

  private final case class Response(status: Int, headers: Map[String, Vector[String]], body: String):
    def header(name: String): Maybe[String] =
      headers.get(name.toLowerCase).flatMap(_.headOption) match
        case Some(value) => Present(value)
        case None        => Absent

    def all(name: String): Vector[String] = headers.getOrElse(name.toLowerCase, Vector.empty)

    def secure: Boolean =
      header("Cache-Control") == Present("no-store") &&
        header("Referrer-Policy") == Present("no-referrer") &&
        !headers.keys.exists(_.startsWith("access-control-"))

    override def toString: String =
      s"Response($status, headers=${headers.keys.toVector.sorted.mkString(",")}, body=<redacted>)"

  private def start(
      launcher: FakeLauncher,
      config: WebHost.Config = WebHost.Config()
  )(using Frame): WebHost.BoundHost < (Async & Scope & Abort[WebHost.WebHostError]) =
    WebHost.start(
      config,
      launcher,
      GitHubTokenVerifier.recorded("""{"data":{"viewer":{"login":"octocat"}}}"""),
      Absent
    )

  private def request(
      port: Int,
      method: String,
      path: String,
      headers: Seq[(String, String)],
      body: String = ""
  )(using Frame): Response < Async = Async.defer {
    val socket = new Socket("127.0.0.1", port)
    try
      socket.setSoTimeout(5000)
      val bytes       = body.getBytes(StandardCharsets.UTF_8)
      val wireHeaders =
        headers ++ Seq("Connection" -> "close") ++
          (if body.nonEmpty then Seq("Content-Length" -> bytes.length.toString) else Seq.empty)
      val head =
        s"$method $path HTTP/1.1\r\n" + wireHeaders.map((name, value) => s"$name: $value\r\n").mkString + "\r\n"
      val out = socket.getOutputStream
      out.write(head.getBytes(StandardCharsets.ISO_8859_1))
      out.write(bytes)
      out.flush()
      readResponse(socket, method == "HEAD")
    finally socket.close()
  }

  private def readResponse(socket: Socket, headRequest: Boolean): Response =
    val in      = BufferedInputStream(socket.getInputStream)
    val rawHead = readUntilHeaderEnd(in)
    val lines   = rawHead.split("\r\n").toVector
    val status  = lines.head.split(' ')(1).toInt
    val headers = lines.tail.foldLeft(Map.empty[String, Vector[String]]) { (current, line) =>
      val split = line.indexOf(':')
      if split < 1 then current
      else
        val name  = line.substring(0, split).toLowerCase
        val value = line.substring(split + 1).trim
        current.updated(name, current.getOrElse(name, Vector.empty) :+ value)
    }
    val body = if headRequest then ""
    else
      headers.get("content-length").flatMap(_.headOption).map(_.toInt) match
        case Some(length) => new String(in.readNBytes(length), StandardCharsets.UTF_8)
        case None if headers.get("transfer-encoding").exists(_.exists(_.equalsIgnoreCase("chunked"))) =>
          readChunked(in)
        case None => new String(in.readAllBytes(), StandardCharsets.UTF_8)
    Response(status, headers, body)

  private def readUntilHeaderEnd(in: BufferedInputStream): String =
    val bytes = ByteArrayOutputStream()
    var state = 0
    while state < 4 do
      val next = in.read()
      if next < 0 then throw new IllegalStateException("response ended before headers")
      bytes.write(next)
      state = (state, next) match
        case (0, 13) => 1
        case (1, 10) => 2
        case (2, 13) => 3
        case (3, 10) => 4
        case (_, 13) => 1
        case _       => 0
    new String(bytes.toByteArray, StandardCharsets.ISO_8859_1).stripSuffix("\r\n\r\n")

  private def readChunked(in: BufferedInputStream): String =
    val bytes = ByteArrayOutputStream()
    var done  = false
    while !done do
      val length = readLine(in).takeWhile(_ != ';').trim.toIntOption.getOrElse(0)
      if length == 0 then
        done = true
        val _ = readLine(in)
      else
        bytes.write(in.readNBytes(length))
        val _ = readLine(in)
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def readLine(in: BufferedInputStream): String =
    val bytes = ByteArrayOutputStream()
    var cr    = false
    var done  = false
    while !done do
      val next = in.read()
      if next < 0 then done = true
      else if cr && next == 10 then done = true
      else
        if cr then bytes.write(13)
        cr = next == 13
        if !cr then bytes.write(next)
    new String(bytes.toByteArray, StandardCharsets.ISO_8859_1)

  private def exactHeaders(origin: String): Seq[(String, String)] =
    Seq("Host" -> origin.stripPrefix("http://"), "Origin" -> origin, "Content-Type" -> "application/json")

  private def launchFrom(url: String): String =
    url.split("#launch=", 2) match
      case Array(_, value) => value
      case _               => throw new AssertionError("missing launch fragment")

  private def sessionCookie(response: Response): String =
    response.header("Set-Cookie") match
      case Present(value) => value.takeWhile(_ != ';')
      case Absent         => throw new AssertionError("missing session cookie")

  private def exchange(host: WebHost.BoundHost, launch: String)(using Frame): Response < Async =
    request(
      host.port,
      "POST",
      "/api/session/exchange",
      exactHeaders(host.origin),
      s"""{"launch":"$launch"}"""
    )

  private def statusRpc(
      host: WebHost.BoundHost,
      cookie: String,
      overrides: Seq[(String, String)] = Seq.empty,
      omitted: Set[String] = Set.empty
  )(using Frame): Response < Async =
    val defaults = exactHeaders(host.origin) :+ ("Cookie" -> cookie)
    val replaced = overrides.map(_._1.toLowerCase).toSet
    request(
      host.port,
      "POST",
      "/api/jsonrpc",
      defaults.filterNot((name, _) => replaced.contains(name.toLowerCase) || omitted.contains(name)) ++ overrides,
      """{"jsonrpc":"2.0","id":1,"method":"morphir/github/status","params":{}}"""
    )

  "WebHost" - {

    "completes bridge release after its release fiber receives interruption" in {
      for
        closeStarted  <- Latch.init(1)
        allowClose    <- Latch.init(1)
        closeFinished <- Latch.init(1)
        fiber         <- Fiber.initUnscoped {
          WebHost.releaseBridge(
            closeStarted.release.andThen(allowClose.await).andThen(closeFinished.release)
          )
        }
        _        <- closeStarted.await
        _        <- fiber.interrupt
        _        <- allowClose.release
        finished <- Fiber.initUnscoped(closeFinished.await).map(_.block(1.second))
        result   <- fiber.block(1.second)
      yield assert(finished.isSuccess && result.isPanic)
    }

    "rejects ports outside 0 through 65535 with safe errors" in {
      val launcher = FakeLauncher()
      Scope.run {
        Abort.run[WebHost.WebHostError](start(launcher, WebHost.Config(port = -1))).map { low =>
          Abort.run[WebHost.WebHostError](start(launcher, WebHost.Config(port = 65536))).map { high =>
            assert(low.isFailure && high.isFailure)
            assert(!low.toString.contains("launch=") && !high.toString.contains("launch="))
          }
        }
      }
    }

    "binds an ephemeral socket only at the numeric IPv4 loopback and launches one fragment URL" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          Async.defer {
            val url = launcher.urls.headOption.getOrElse("")
            assert(host.port > 0)
            assert(host.origin == s"http://127.0.0.1:${host.port}")
            assert(url == s"${host.origin}/#launch=${launchFrom(url)}")
            assert(launchFrom(url).matches("[A-Za-z0-9_-]{43}"))
            assert(launcher.urls.size == 1)
            assert(host.toString.contains(host.origin))
            assert(!host.toString.contains(launchFrom(url)))
          }
        }
      }
    }

    "exchanges once, sets the exact cookie flags, and secures static and API responses" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          val launch = launchFrom(launcher.urls.head)
          exchange(host, launch).map { accepted =>
            exchange(host, launch).map { replay =>
              request(
                host.port,
                "GET",
                "/",
                Seq("Host" -> s"127.0.0.1:${host.port}", "Cookie" -> sessionCookie(accepted))
              ).map { html =>
                val cookie = accepted.header("Set-Cookie")
                assert(accepted.status == 204 && accepted.body.isEmpty && accepted.secure)
                assert(cookie.exists(_.matches("morphir_session=[A-Za-z0-9_-]{43}; HttpOnly; SameSite=Strict; Path=/")))
                assert(replay.status == 401 && replay.secure && !replay.body.contains(launch))
                assert(html.status == 200 && html.secure)
                assert(html.header("Content-Security-Policy") == Present(csp))
                assert(html.header("Content-Type").exists(_.startsWith("text/html")))
                assert(html.body.contains("<div id=\"app\"></div>"))
                assert(!html.body.contains(launch))
                assert(html.header("Location").isEmpty)
              }
            }
          }
        }
      }
    }

    "controls unsupported and unmatched API requests without CORS" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          exchange(host, launchFrom(launcher.urls.head)).map { accepted =>
            val cookie      = sessionCookie(accepted)
            val authorized  = exactHeaders(host.origin) :+ ("Cookie" -> cookie)
            val unsupported = Seq(
              ("OPTIONS", "/api/jsonrpc", authorized, ""),
              ("GET", "/api/jsonrpc", authorized, ""),
              ("PUT", "/api/jsonrpc", authorized, "{}"),
              ("HEAD", "/api/jsonrpc", authorized, ""),
              ("TRACE", "/api/jsonrpc", authorized, ""),
              ("GET", "/api/session/exchange", exactHeaders(host.origin), "")
            )
            Kyo.foreach(unsupported) { (method, path, headers, body) =>
              request(host.port, method, path, headers, body)
            }.map { unsupportedResponses =>
              val policyFailures = Seq(
                Seq("Host" -> s"127.0.0.1:${host.port}"),
                Seq("Host" -> s"127.0.0.1:${host.port}", "Origin" -> host.origin),
                authorized.filterNot(_._1 == "Cookie"),
                authorized.filterNot(_._1 == "Cookie") :+ ("Cookie" -> "morphir_session=wrong")
              )
              Kyo.foreach(policyFailures)(headers => request(host.port, "GET", "/api/jsonrpc", headers)).map {
                rejectedByPolicy =>
                  val unmatched = Seq(
                    ("POST", "/api/not-registered", Seq("Host" -> s"127.0.0.1:${host.port}")),
                    ("OPTIONS", "/api/not-registered", Seq("Host" -> s"127.0.0.1:${host.port}")),
                    ("HEAD", "/api/not-registered", Seq("Host" -> s"127.0.0.1:${host.port}")),
                    ("TRACE", "/api/not-registered", Seq("Host" -> s"127.0.0.1:${host.port}")),
                    ("POST", "/api", Seq("Host" -> s"127.0.0.1:${host.port}")),
                    ("OPTIONS", "/api", Seq("Host" -> s"127.0.0.1:${host.port}"))
                  )
                  Kyo.foreach(unmatched) { (method, path, headers) => request(host.port, method, path, headers) }.map {
                    unmatchedResponses =>
                      assert(unsupportedResponses.forall(response => response.status == 405 && response.secure))
                      assert(rejectedByPolicy.map(_.status) == Seq(401, 400, 401, 401))
                      assert(rejectedByPolicy.forall(_.secure))
                      assert(unmatchedResponses.forall(response => response.status == 404 && response.secure))
                      assert((unsupportedResponses ++ rejectedByPolicy ++ unmatchedResponses).forall(response =>
                        !response.headers.keys.exists(_.startsWith("access-control-"))
                      ))
                  }
              }
            }
          }
        }
      }
    }

    "accepts authenticated GitHub RPC and rejects every Host Origin cookie and content-type mismatch" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          exchange(host, launchFrom(launcher.urls.head)).map { accepted =>
            val cookie = sessionCookie(accepted)
            statusRpc(host, cookie).map { ok =>
              val submittedToken = "integration-token-sentinel"
              val connectParams  = Json.encode(ConnectRequest(TokenSubmission.from(submittedToken), remember = false))
              request(
                host.port,
                "POST",
                "/api/jsonrpc",
                exactHeaders(host.origin) :+ ("Cookie" -> cookie),
                s"""{"jsonrpc":"2.0","id":2,"method":"morphir/github/connect","params":$connectParams}"""
              ).map { connected =>
                val denied = Seq(
                  Seq("Host" -> s"localhost:${host.port}")              -> Set.empty[String],
                  Seq("Host" -> s"[::1]:${host.port}")                  -> Set.empty[String],
                  Seq("Origin" -> "http://foreign.test")                -> Set.empty[String],
                  Seq.empty[(String, String)]                           -> Set("Origin"),
                  Seq("Origin" -> host.origin, "Origin" -> host.origin) -> Set.empty[String],
                  Seq("Cookie" -> "morphir_session=wrong")              -> Set.empty[String],
                  Seq.empty[(String, String)]                           -> Set("Cookie"),
                  Seq("Cookie" -> cookie, "Cookie" -> cookie)           -> Set.empty[String],
                  Seq("Content-Type" -> "text/plain")                   -> Set.empty[String],
                  Seq.empty[(String, String)]                           -> Set("Content-Type"),
                  Seq("Content-Type" -> "application/json", "Content-Type" -> "application/json") -> Set.empty[String]
                )
                Kyo.foreach(denied)((headers, omitted) => statusRpc(host, cookie, headers, omitted)).map { responses =>
                  val summary = responses.zipWithIndex.map { (response, index) =>
                    (index, response.status, response.header("Cache-Control"), response.header("Referrer-Policy"))
                  }
                  assert(ok.status == 200 && ok.secure)
                  assert(ok.header("Content-Type").exists(_.startsWith("application/json")))
                  assert(ok.body.contains("Disconnected"))
                  assert(connected.status == 200 && connected.secure)
                  assert(!connected.body.contains(submittedToken))
                  assert(summary.forall { (_, status, _, _) => Set(400, 401).contains(status) })
                  assert(responses.forall(_.secure))
                  assert((ok +: connected +: responses).forall(response =>
                    !response.body.contains(cookie) && response.secure
                  ))
                }
              }
            }
          }
        }
      }
    }

    "serves only canonical classpath assets and rejects traversal variants" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          val hostHeader = Seq("Host" -> s"127.0.0.1:${host.port}")
          request(host.port, "GET", "/index.html", hostHeader).map { index =>
            request(host.port, "GET", "/assets/morphir-web-renderer.js", hostHeader).map { script =>
              request(host.port, "GET", "/assets/morphir-web.css", hostHeader).map { styles =>
                val denied = Seq(
                  "/../package.mill.yaml",
                  "/%2e%2e/package.mill.yaml",
                  "/%252e%252e/package.mill.yaml",
                  "/..%2fpackage.mill.yaml",
                  "/%252e%252e%252fpackage.mill.yaml",
                  "/..\\package.mill.yaml",
                  "/%5c..%5cpackage.mill.yaml",
                  "/%255c..%255cpackage.mill.yaml",
                  "/%2500package.mill.yaml",
                  "/assets/morphir-web-renderer.js.map",
                  "/package.mill.yaml"
                )
                Kyo.foreach(denied)(path => request(host.port, "GET", path, hostHeader)).map { rejected =>
                  val summary = rejected.zipWithIndex.map { (response, index) =>
                    (index, response.status, response.header("Cache-Control"), response.header("Referrer-Policy"))
                  }
                  assert(index.status == 200 && index.secure)
                  assert(script.status == 200 && script.secure)
                  assert(script.header("Content-Type").exists(_.startsWith("text/javascript")))
                  assert(!script.body.contains("sourceMappingURL"))
                  assert(styles.status == 200 && styles.secure)
                  assert(styles.header("Content-Type").exists(_.startsWith("text/css")))
                  val requiredSelectors = Seq(
                    ".settings-group",
                    ".settings-group-title",
                    ".settings-intro",
                    ".github-connection",
                    ".github-connection-status",
                    ".github-connection-detail",
                    ".github-connection-form",
                    ".github-token-label",
                    ".github-token-input",
                    ".github-remember",
                    ".github-connection-actions",
                    ".github-connection-action",
                    ".github-connection-secondary",
                    ".github-connection-progress",
                    ".github-connection-error"
                  )
                  val requiredTokens = Seq(
                    "--bg:",
                    "--surface:",
                    "--panel:",
                    "--panel-edge:",
                    "--text:",
                    "--muted:",
                    "--muted2:",
                    "--accent:",
                    "--accent-text:",
                    "--knob:"
                  )
                  assert(requiredSelectors.forall(styles.body.contains))
                  assert(requiredTokens.forall(styles.body.contains))
                  assert(summary.forall { (_, status, _, _) => Set(400, 404).contains(status) })
                  assert(rejected.forall(_.secure))
                  assert(rejected.forall(response => !response.body.contains("moduleDeps")))
                }
              }
            }
          }
        }
      }
    }

    "rejects localhost and IPv6 Host values on static routes" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          val hosts = Seq(s"localhost:${host.port}", s"[::1]:${host.port}")
          Kyo.foreach(hosts)(value => request(host.port, "GET", "/", Seq("Host" -> value))).map { responses =>
            assert(responses.forall(response => response.status == 401 && response.secure))
          }
        }
      }
    }

    "rejects duplicate or missing Host, encoded NUL, and oversized bodies before application routes" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher).map { host =>
          exchange(host, launchFrom(launcher.urls.head)).map { accepted =>
            val cookie = sessionCookie(accepted)
            statusRpc(
              host,
              cookie,
              Seq("Host" -> s"127.0.0.1:${host.port}", "Host" -> s"127.0.0.1:${host.port}")
            ).map { duplicateHost =>
              statusRpc(host, cookie, omitted = Set("Host")).map { missingHost =>
                request(
                  host.port,
                  "GET",
                  "/%00package.mill.yaml",
                  Seq("Host" -> s"127.0.0.1:${host.port}")
                ).map { encodedNul =>
                  val oversized = "x" * (RequestPolicy.maxRequestBytes + 1)
                  request(
                    host.port,
                    "POST",
                    "/api/session/exchange",
                    exactHeaders(host.origin),
                    oversized
                  ).map { tooLarge =>
                    val parserRejections = Seq(duplicateHost, missingHost, encodedNul)
                    assert(parserRejections.forall(_.status == 400))
                    assert(tooLarge.status == 413)
                    assert((parserRejections :+ tooLarge).forall(response =>
                      !response.headers.keys.exists(_.startsWith("access-control-"))
                    ))
                  }
                }
              }
            }
          }
        }
      }
    }

    "expires the launch credential and the established session" in {
      val sessionTtl = 1.minute
      Clock.withTimeControl { clock =>
        val launcher = FakeLauncher()
        Scope.run {
          start(launcher, WebHost.Config(sessionTtl = sessionTtl)).map { host =>
            val launch = launchFrom(launcher.urls.head)
            clock.advance(sessionTtl).andThen(exchange(host, launch)).map { expiredLaunch =>
              assert(expiredLaunch.status == 401 && expiredLaunch.secure)
            }
          }
        }.andThen {
          val sessionLauncher = FakeLauncher()
          Scope.run {
            start(sessionLauncher, WebHost.Config(sessionTtl = sessionTtl)).map { host =>
              exchange(host, launchFrom(sessionLauncher.urls.head)).map { accepted =>
                val cookie = sessionCookie(accepted)
                clock.advance(sessionTtl).andThen(statusRpc(host, cookie)).map { expiredSession =>
                  request(
                    host.port,
                    "GET",
                    "/",
                    Seq("Host" -> s"127.0.0.1:${host.port}", "Cookie" -> cookie)
                  ).map { recovery =>
                    val freshLaunch = recovery.header("Location") match
                      case Present(value) => launchFrom(value)
                      case Absent         => ""
                    request(host.port, "GET", "/", Seq("Host" -> s"127.0.0.1:${host.port}")).map { html =>
                      exchange(host, freshLaunch).map { refreshed =>
                        exchange(host, launchFrom(sessionLauncher.urls.head)).map { oldReplay =>
                          statusRpc(host, sessionCookie(refreshed)).map { restored =>
                            assert(expiredSession.status == 401 && expiredSession.secure)
                            assert(recovery.status == 302 && recovery.secure)
                            assert(freshLaunch.matches("[A-Za-z0-9_-]{43}"))
                            assert(html.status == 200 && html.secure)
                            assert(refreshed.status == 204 && refreshed.secure)
                            assert(oldReplay.status == 401 && oldReplay.secure)
                            assert(restored.status == 200 && restored.secure)
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

    "keeps no-open usable through one loopback redirect without calling the launcher" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher, WebHost.Config(openBrowser = false)).map { host =>
          val hostHeader = Seq("Host" -> s"127.0.0.1:${host.port}")
          request(host.port, "GET", "/", hostHeader).map { redirect =>
            val location = redirect.header("Location")
            val launch   = location match
              case Present(value) => launchFrom(value)
              case Absent         => ""
            request(host.port, "GET", "/", hostHeader).map { html =>
              exchange(host, launch).map { accepted =>
                assert(launcher.urls.isEmpty)
                assert(redirect.status == 302 && redirect.secure)
                assert(location == Present(s"/#launch=$launch"))
                assert(launch.matches("[A-Za-z0-9_-]{43}"))
                assert(html.status == 200 && html.secure && !html.body.contains(launch))
                assert(accepted.status == 204 && accepted.secure)
              }
            }
          }
        }
      }
    }

    "mints the no-open launch only when the first valid root request arrives" in {
      val launcher = FakeLauncher()
      Scope.run {
        start(launcher, WebHost.Config(openBrowser = false, sessionTtl = 120.millis)).map { host =>
          Async.sleep(180.millis).andThen {
            request(host.port, "GET", "/", Seq("Host" -> s"127.0.0.1:${host.port}")).map { redirect =>
              val launch = redirect.header("Location") match
                case Present(value) => launchFrom(value)
                case Absent         => ""
              exchange(host, launch).map { accepted =>
                assert(launcher.urls.isEmpty)
                assert(redirect.status == 302 && redirect.secure)
                assert(accepted.status == 204 && accepted.secure)
              }
            }
          }
        }
      }
    }

    "keeps the bound host alive when the browser launcher panics" in {
      val launcher = FakeLauncher(panicOnOpen = true)
      Scope.run {
        Abort.run[WebHost.WebHostError](start(launcher)).map { started =>
          started match
            case Result.Success(host) =>
              request(host.port, "GET", "/", Seq("Host" -> s"127.0.0.1:${host.port}")).map { recovery =>
                assert(launcher.urls.size == 1)
                assert(recovery.status == 302 && recovery.secure)
                assert(!started.toString.contains("launch="))
              }
            case _ => assert(false)
        }
      }
    }

    "switches a false browser result to a fresh manual launch" in {
      val launcher = FakeLauncher(opened = false)
      Scope.run {
        start(launcher).map { host =>
          val hostHeader = Seq("Host" -> s"127.0.0.1:${host.port}")
          request(host.port, "GET", "/", hostHeader).map { redirect =>
            val launch = redirect.header("Location") match
              case Present(value) => launchFrom(value)
              case Absent         => ""
            exchange(host, launch).map { accepted =>
              assert(launcher.urls.size == 1)
              assert(redirect.status == 302 && redirect.secure)
              assert(redirect.header("Location") == Present(s"/#launch=$launch"))
              assert(launch.matches("[A-Za-z0-9_-]{43}"))
              assert(accepted.status == 204 && accepted.secure)
              assert(!launcher.toString.contains(launch))
            }
          }
        }
      }
    }
  }
end WebHostTests

package org.finos.morphir.cli

import java.io.{BufferedInputStream, ByteArrayOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import kyo.*
import kyo.test.*
import morphir.connector.github.GitHubTokenVerifier
import morphir.web.server.WebHost

class ServerCommandWebHostTests extends Test[Any]:

  private final case class Response(status: Int, headers: Map[String, String])

  private def request(
      port: Int,
      method: String,
      path: String,
      headers: Seq[(String, String)],
      body: String = ""
  )(using Frame): Response < Async = Async.defer {
    val socket = Socket("127.0.0.1", port)
    try
      socket.setSoTimeout(5000)
      val bodyBytes = body.getBytes(StandardCharsets.UTF_8)
      val head      =
        s"$method $path HTTP/1.1\r\n" +
          (headers ++ Seq("Connection" -> "close", "Content-Length" -> bodyBytes.length.toString))
            .map((name, value) => s"$name: $value\r\n")
            .mkString +
          "\r\n"
      val out = socket.getOutputStream
      out.write(head.getBytes(StandardCharsets.ISO_8859_1))
      out.write(bodyBytes)
      out.flush()
      readResponse(socket)
    finally socket.close()
  }

  private def readResponse(socket: Socket): Response =
    val bytes = ByteArrayOutputStream()
    val in    = BufferedInputStream(socket.getInputStream)
    var state = 0
    while state < 4 do
      val next = in.read()
      if next < 0 then throw IllegalStateException("response ended before headers")
      bytes.write(next)
      state = (state, next) match
        case (0, 13) => 1
        case (1, 10) => 2
        case (2, 13) => 3
        case (3, 10) => 4
        case (_, 13) => 1
        case _       => 0
    val lines = new String(bytes.toByteArray, StandardCharsets.ISO_8859_1)
      .stripSuffix("\r\n\r\n")
      .split("\r\n")
      .toVector
    val headers = lines.tail.flatMap { line =>
      val split = line.indexOf(':')
      if split < 1 then None
      else Some(line.substring(0, split).toLowerCase -> line.substring(split + 1).trim)
    }.toMap
    Response(lines.head.split(' ')(1).toInt, headers)

  private def launchFrom(location: String): String =
    location.split("#launch=", 2) match
      case Array(_, launch) => launch
      case _                => throw AssertionError("missing launch fragment")

  "ServerCommand live WebHost composition" - {
    "keeps serving a manual launch after the desktop browser fails safely" in {
      val failureText = "desktop-launch-secret-sentinel"
      val desktop     = new ServerCommand.DesktopPlatform:
        def browse(url: String): Unit = throw RuntimeException(failureText)
      val launcher = ServerCommand.DesktopBrowserLauncher(desktop, ServerCommand.Output.console)
      val verifier = GitHubTokenVerifier.recorded("""{"data":{"viewer":{"login":"octocat"}}}""")

      Console.withOut {
        Scope.run {
          WebHost
            .startWithLauncher(WebHost.Config(), url => launcher.open(url), verifier, Absent)
            .map { host =>
              val hostHeader = Seq("Host" -> s"127.0.0.1:${host.port}")
              request(host.port, "GET", "/", hostHeader).map { redirect =>
                val location = redirect.headers.getOrElse("location", "")
                val launch   = launchFrom(location)
                request(
                  host.port,
                  "POST",
                  "/api/session/exchange",
                  Seq(
                    "Host"         -> s"127.0.0.1:${host.port}",
                    "Origin"       -> host.origin,
                    "Content-Type" -> "application/json"
                  ),
                  s"""{"launch":"$launch"}"""
                ).map { accepted =>
                  assert(redirect.status == 302)
                  assert(location == s"/#launch=$launch")
                  assert(launch.matches("[A-Za-z0-9_-]{43}"))
                  assert(accepted.status == 204)
                }
              }
            }
        }
      }.map { case (output, _) =>
        assert(output.stdOut.isEmpty)
        assert(output.stdErr == s"${ServerCommand.browserWarning}\n")
        assert(!output.toString.contains("#launch="))
        assert(!output.toString.contains(failureText))
        assert(!output.toString.contains(classOf[RuntimeException].getName))
      }
    }
  }
end ServerCommandWebHostTests

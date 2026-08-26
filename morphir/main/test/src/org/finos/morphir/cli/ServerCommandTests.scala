package org.finos.morphir.cli

import caseapp.core.parser.Parser
import caseapp.core.app.Command
import java.awt.HeadlessException
import java.net.URISyntaxException
import kyo.*
import kyo.test.*

class ServerCommandTests extends Test[Any]:

  private val launchSentinel = "http://127.0.0.1:43123/#launch=launch-secret-sentinel"

  private final class FakeLauncher extends ServerCommand.BrowserLauncher:
    private var opened = Vector.empty[String]

    def open(url: String)(using Frame): Boolean < Async = Async.defer {
      opened = opened :+ url
      true
    }

    def urls: Vector[String] = opened

  private final class FakeBoundHost(events: collection.mutable.ArrayBuffer[String], failAwait: Boolean)
      extends ServerCommand.BoundHost:
    val port = 43123

    def await(using Frame): Unit < Async = Async.defer {
      events += "await"
      if failAwait then throw new RuntimeException("await-secret-sentinel")
    }

  private final class FakeHost(
      failStart: Boolean = false,
      failAwait: Boolean = false
  ) extends ServerCommand.Host:
    val events  = collection.mutable.ArrayBuffer.empty[String]
    var configs = Vector.empty[ServerCommand.HostConfig]

    def start(
        config: ServerCommand.HostConfig,
        browserLauncher: ServerCommand.BrowserLauncher
    )(using Frame): ServerCommand.BoundHost < (Async & Scope & Abort[Throwable]) =
      Sync.defer {
        events += "start"
        configs = configs :+ config
      }.andThen {
        if failStart then Abort.fail(new RuntimeException("host-secret-sentinel"))
        else
          val launch = if config.openBrowser then browserLauncher.open(launchSentinel).unit else Kyo.unit
          launch.andThen(
            Scope.acquireRelease(Sync.defer(FakeBoundHost(events, failAwait))) { _ =>
              Sync.defer(events += "release")
            }
          )
      }

  private final class ThrowingDesktop(failure: Throwable) extends ServerCommand.DesktopPlatform:
    def browse(url: String): Unit = throw failure

  private def dependencies(host: FakeHost, launcher: ServerCommand.BrowserLauncher = FakeLauncher()) =
    ServerCommand.Dependencies(host, launcher, ServerCommand.Output.console)

  private def runCommand(options: ServerOptions, dependencies: ServerCommand.Dependencies) =
    Console.withOut(Abort.run[Throwable](ServerCommand.run(options, dependencies)))

  private def parse(args: String*) =
    Parser[ServerOptions].parse(args)

  "ServerOptions parsing" - {
    "uses an ephemeral port and opens the browser by default" in
      assert(parse() == Right((ServerOptions(), Seq.empty)))

    "accepts an explicit port" in
      assert(parse("--port", "8123") == Right((ServerOptions(port = 8123), Seq.empty)))

    "accepts both ends of the TCP port range" in {
      assert(parse("--port", "0") == Right((ServerOptions(port = 0), Seq.empty)))
      assert(parse("--port", "65535") == Right((ServerOptions(port = 65535), Seq.empty)))
    }

    "accepts --no-open" in
      assert(parse("--no-open") == Right((ServerOptions(noOpen = true), Seq.empty)))

    "rejects ports outside the TCP range" in {
      assert(parse("--port", "-1").isLeft)
      assert(parse("--port", "65536").isLeft)
    }

    "rejects malformed ports" in
      assert(parse("--port", "not-a-port").isLeft)
  }

  "command registration" - {
    "adds server without changing the legacy command names" in {
      val commandNames = MorphirCliMain.commands.flatMap(_.names).map(_.mkString(" "))
      assert(
        commandNames == Seq(
          "bundle",
          "develop",
          "library",
          "setup",
          "test",
          "version",
          "server",
          "elm",
          "elm develop",
          "elm init",
          "elm make",
          "elm restore",
          "elm test"
        )
      )
    }

    "keeps the registered server command free of process signal ownership" in {
      val command: Any = ServerCommand
      assert(command.isInstanceOf[Command[?]])
      assert(!command.isInstanceOf[KyoCommand[?]])
    }
  }

  "web host availability" - {
    "treats an absent implementation class as unavailable" in {
      val missing = new ClassLoader(null):
        override protected def loadClass(name: String, resolve: Boolean): Class[?] =
          throw ClassNotFoundException(name)

      assert(!ServerCommand.webHostAvailable(missing))
    }

    "propagates linkage failures from the implementation class" in {
      val failure = LinkageError("web-host-linkage-sentinel")
      val broken  = new ClassLoader(null):
        override protected def loadClass(name: String, resolve: Boolean): Class[?] = throw failure

      var observed: Throwable | Null = null
      try ServerCommand.webHostAvailable(broken)
      catch case error: Throwable => observed = error

      assert(observed eq failure)
    }
  }

  "ServerCommand.run" - {
    "starts on the requested port, opens by default, reports only the bound origin, awaits, and releases" in {
      val host     = FakeHost()
      val launcher = FakeLauncher()
      runCommand(ServerOptions(), dependencies(host, launcher)).map { case (output, result) =>
        assert(result == Result.Success(()))
        assert(host.configs == Vector(ServerCommand.HostConfig(port = 0, openBrowser = true)))
        assert(launcher.urls == Vector(launchSentinel))
        assert(host.events.toVector == Vector("start", "await", "release"))
        assert(output.stdOut == "Listening on http://127.0.0.1:43123\n")
        assert(output.stdErr.isEmpty)
        assert(!output.toString.contains("#launch="))
        assert(!output.toString.contains("launch-secret-sentinel"))
        assert(!output.toString.contains("morphir_session"))
      }
    }

    "passes --no-open through without touching the browser launcher" in {
      val host     = FakeHost()
      val launcher = FakeLauncher()
      runCommand(ServerOptions(port = 8123, noOpen = true), dependencies(host, launcher)).map {
        case (output, result) =>
          assert(result == Result.Success(()))
          assert(host.configs == Vector(ServerCommand.HostConfig(port = 8123, openBrowser = false)))
          assert(launcher.urls.isEmpty)
          assert(host.events.toVector == Vector("start", "await", "release"))
          assert(output == Console.Out("Listening on http://127.0.0.1:43123\n", ""))
      }
    }

    "keeps awaiting after the desktop browser launch fails" in {
      val host     = FakeHost()
      val launcher = ServerCommand.DesktopBrowserLauncher(
        ThrowingDesktop(RuntimeException("launch-secret-sentinel")),
        ServerCommand.Output.console
      )
      runCommand(ServerOptions(), dependencies(host, launcher)).map { case (output, result) =>
        assert(result == Result.Success(()))
        assert(host.events.toVector == Vector("start", "await", "release"))
        assert(
          output == Console.Out(
            "Listening on http://127.0.0.1:43123\n",
            s"${ServerCommand.browserWarning}\n"
          )
        )
        assert(!output.toString.contains("#launch="))
        assert(!output.toString.contains("launch-secret-sentinel"))
      }
    }

    "rejects directly constructed invalid options before starting the host" in {
      val lowHost  = FakeHost()
      val highHost = FakeHost()
      runCommand(ServerOptions(port = -1), dependencies(lowHost)).map { case (lowOutput, lowResult) =>
        runCommand(ServerOptions(port = 65536), dependencies(highHost)).map { case (highOutput, highResult) =>
          assert(lowResult.isFailure)
          assert(highResult.isFailure)
          assert(lowHost.events.isEmpty)
          assert(highHost.events.isEmpty)
          assert(lowOutput == Console.Out("", ""))
          assert(highOutput == Console.Out("", ""))
        }
      }
    }

    "propagates host startup failure without output or an await" in {
      val host = FakeHost(failStart = true)
      runCommand(ServerOptions(noOpen = true), dependencies(host)).map { case (output, result) =>
        assert(result.isFailure)
        assert(host.events.toVector == Vector("start"))
        assert(output == Console.Out("", ""))
      }
    }

    "releases the host when awaiting termination panics" in {
      val host = FakeHost(failAwait = true)
      runCommand(ServerOptions(noOpen = true), dependencies(host)).map { case (output, result) =>
        assert(result.isPanic)
        assert(host.events.toVector == Vector("start", "await", "release"))
        assert(output == Console.Out("Listening on http://127.0.0.1:43123\n", ""))
        assert(!output.toString.contains("await-secret-sentinel"))
      }
    }
  }

  "DesktopBrowserLauncher" - {
    "opens a supported browser without writing output" in {
      var openedUrls = Vector.empty[String]
      val desktop    = new ServerCommand.DesktopPlatform:
        def browse(url: String): Unit = openedUrls = openedUrls :+ url
      val launcher = ServerCommand.DesktopBrowserLauncher(desktop, ServerCommand.Output.console)
      Console.withOut(launcher.open(launchSentinel)).map { case (output, launched) =>
        assert(launched)
        assert(openedUrls == Vector(launchSentinel))
        assert(output == Console.Out("", ""))
      }
    }

    "turns every browser platform failure into one fixed safe warning" in {
      val failures = Seq[Throwable](
        UnsupportedOperationException("unsupported launch-secret-sentinel"),
        SecurityException("security launch-secret-sentinel"),
        HeadlessException("desktop launch-secret-sentinel"),
        URISyntaxException("uri launch-secret-sentinel", "reason launch-secret-sentinel"),
        RuntimeException("runtime launch-secret-sentinel"),
        AssertionError("error launch-secret-sentinel")
      )
      Async.foreachDiscard(failures) { failure =>
        val launcher = ServerCommand.DesktopBrowserLauncher(ThrowingDesktop(failure), ServerCommand.Output.console)
        Console.withOut(launcher.open(launchSentinel)).map { case (output, opened) =>
          assert(!opened)
          assert(output.stdOut.isEmpty)
          assert(output.stdErr == s"${ServerCommand.browserWarning}\n")
          assert(!output.toString.contains("launch-secret-sentinel"))
          assert(!output.toString.contains(failure.getClass.getName))
        }
      }
    }
  }

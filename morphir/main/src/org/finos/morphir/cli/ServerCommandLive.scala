package org.finos.morphir.cli

import kyo.*
import morphir.appkit.SecretVault
import morphir.connector.github.GitHubTokenVerifier
import morphir.web.server.WebHost

private[cli] object ServerCommandLive:

  def dependencies: ServerCommand.Dependencies =
    ServerCommand.Dependencies(
      LiveHost,
      ServerCommand.DesktopBrowserLauncher(ServerCommand.DesktopPlatform.system, ServerCommand.Output.console),
      ServerCommand.Output.console
    )

  private object LiveHost extends ServerCommand.Host:
    def start(
        config: ServerCommand.HostConfig,
        browserLauncher: ServerCommand.BrowserLauncher
    )(using Frame): ServerCommand.BoundHost < (Async & Scope & Abort[Throwable]) =
      val started: AnyRef < (Async & Scope & Abort[Throwable]) = WebHost
        .startWithLauncher(
          WebHost.Config(port = config.port, openBrowser = config.openBrowser),
          url => browserLauncher.open(url),
          GitHubTokenVerifier.live,
          Present(SecretVault.system)
        )
      started.map { value =>
        val port = value.asInstanceOf[WebHost.BoundHost].port
        LiveBoundHost(port, () => value.asInstanceOf[WebHost.BoundHost].await)
      }

  private final class LiveBoundHost(
      val port: Int,
      waitForClose: () => Unit < Async
  ) extends ServerCommand.BoundHost:
    def await(using Frame): Unit < Async = waitForClose()
